;;; funcs.el --- my-pi layer functions file for Spacemacs. -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author: Sebastian Hahn <sebastian.hahn@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)
(require 'comint)

;;; ---------------------------------------------------------------------------
;;; Customization
;;; ---------------------------------------------------------------------------

(defgroup pi nil
  "pi (coding agent) RPC client for Spacemacs."
  :group 'applications
  :prefix "pi-")

(defcustom pi-executable "pi"
  "Executable name or path of the pi CLI."
  :type 'string
  :group 'pi)

(defcustom pi-rpc-args '("--mode" "rpc" "--no-session")
  "Arguments passed to pi when starting an RPC session.

Common additions: `--name NAME', `--provider NAME', `--model ID',
`--session-dir PATH'.  See `pi --help' for the installed version."
  :type '(repeat string)
  :group 'pi)

(defcustom pi-buffer-name "pi"
  "Base name used for pi session buffers."
  :type 'string
  :group 'pi)

(defcustom pi-auto-scroll t
  "If non-nil, keep the window scrolled to the streamed output."
  :type 'boolean
  :group 'pi)

;;; ---------------------------------------------------------------------------
;;; Faces
;;; ---------------------------------------------------------------------------

(defface pi-user-face
  '((((background light)) :inherit bold :foreground "SkyBlue"
     :background "gray90" :extend t)
    (t :inherit bold :foreground "SkyBlue" :background "gray25" :extend t))
  "Face for user prompts (background spans the full line)."
  :group 'pi)

(defface pi-assistant-face
  '((t :inherit default))
  "Face for assistant text."
  :group 'pi)

(defface pi-thinking-face
  '((t :inherit shadow :slant italic))
  "Face for assistant reasoning/thinking text."
  :group 'pi)

(defface pi-tool-face
  '((t :inherit shadow))
  "Face for tool activity and metadata."
  :group 'pi)

(defface pi-error-face
  '((t :foreground "red" :weight bold))
  "Face for errors."
  :group 'pi)

;;; ---------------------------------------------------------------------------
;;; Buffer-local state (declared here to silence byte-compilation warnings)
;;; ---------------------------------------------------------------------------

(defvar pi--process nil)
(defvar pi--stderr-buffer nil)
(defvar pi--session-id nil)
(defvar pi--model-name nil)
(defvar pi--display-name nil)
(defvar pi--status "idle")
(defvar pi--request-counter 0)
(defvar pi--pending nil)
(defvar pi--turn-start nil)
(defvar pi--prompt-marker nil
  "Marker at the start of the editable prompt input area.")
(defvar pi--glyph-marker nil
  "Marker at the start of the read-only prompt glyph \"❯\"; transcript inserts here.")
(defvar pi--stdout-accum "")
(defvar pi--commands nil
  "Cached slash commands of this session, as (NAME . DESCRIPTION).")
(defvar pi--last-session nil
  "Most recently used pi session buffer (global).")
(defvar pi-prompt-history  '(""))
(defvar pi-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Commands use non-letter keys so typing a prompt is unaffected.
    (define-key map [return] #'pi-send)
    (define-key map (kbd "C-<return>") #'pi-send)
    (define-key map (kbd "C-j") #'pi-newline-input)
    (define-key map (kbd "TAB") #'completion-at-point)
    (define-key map (kbd "C-c C-c") #'pi-send)
    (define-key map (kbd "C-c C-a") #'pi-abort)
    (define-key map (kbd "C-c C-n") #'pi-new-session)
    (define-key map (kbd "C-c C-m") #'pi-select-model)
    (define-key map (kbd "C-c C-t") #'pi-set-thinking-level)
    (define-key map (kbd "C-c C-b") #'pi-bash)
    (define-key map (kbd "C-c C-g") #'pi-refresh)
    (define-key map (kbd "C-c C-q") #'pi-quit)
    map))

;;; ---------------------------------------------------------------------------
;;; Major mode
;;; ---------------------------------------------------------------------------

(define-derived-mode pi-mode fundamental-mode "π"
  "Major mode for a pi (coding agent) RPC session buffer.

The transcript above is read-only; the prompt is typed into the editable
line at the bottom.  RET (or `C-c C-c') sends, `C-j' inserts a newline for
multiline prompts.  Other session commands are on the SPC leader
(`SPC $ p ...') or via `C-c C-<key>' (a abort, n new session,
m select model, t thinking level, b bash, g refresh, q quit)."
  (setq-local buffer-read-only nil)
  (setq-local pi--process nil)
  (setq-local pi--stderr-buffer nil)
  (setq-local pi--session-id nil)
  (setq-local pi--model-name nil)
  (setq-local pi--display-name nil)
  (setq-local pi--status "idle")
  (setq-local pi--request-counter 0)
  (setq-local pi--pending nil)
  (setq-local pi--turn-start nil)
  (setq-local pi--prompt-marker nil)
  (setq-local pi--glyph-marker nil)
  (setq-local pi--stdout-accum "")
  (setq-local pi--commands nil)
  (add-hook 'completion-at-point-functions #'pi--command-capf nil t)
  (setq-local header-line-format '(:eval (pi--header-line))))

;; Start pi buffers in Evil insert state so the prompt is typeable and ESC
;; switches to normal state.
(with-eval-after-load 'evil
  (add-to-list 'evil-insert-state-modes 'pi-mode))

(defun pi--header-line ()
  "Return the header line for a pi session buffer."
  (format " π %s · %s · %s"
          (or pi--display-name (or pi--model-name "no model"))
          pi--status
          (or pi--session-id "")))

;;; ---------------------------------------------------------------------------
;;; Buffer helpers
;;; ---------------------------------------------------------------------------

(defun pi--session-buffer ()
  "Return a live pi session buffer, or signal if none exists."
  (cond
   ((and (eq major-mode 'pi-mode) pi--process (current-buffer))
    (current-buffer))
   ((and (buffer-live-p pi--last-session)
         (buffer-local-value 'pi--process pi--last-session))
    pi--last-session)
   ((cl-some (lambda (b)
               (and (buffer-live-p b)
                    (buffer-local-value 'pi--process b)
                    b))
             (buffer-list)))
   (t (user-error "No pi session.  Run `pi-start' first."))))

(defun pi--scroll-to-end ()
  "Scroll the window displaying the current buffer to the end."
  (let ((win (get-buffer-window (current-buffer) t)))
    (when win
      (with-selected-window win
        (goto-char (point-max))))))

(defun pi--ensure-input ()
  "Create the editable prompt line at the bottom of the buffer if needed.

The prompt line is a read-only \"❯\" glyph followed by an editable space and
then the input region.  Transcript is inserted above the glyph via
`pi--glyph-marker'; the editable input starts at `pi--prompt-marker'."
  (unless (and pi--prompt-marker
               (marker-position pi--prompt-marker)
               (eq (marker-buffer pi--prompt-marker) (current-buffer)))
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (let ((beg (point)))
        ;; Blank line between the transcript and the prompt glyph.
        (insert (propertize "\n❯" 'face 'pi-tool-face 'read-only t))
        ;; An editable space right after the glyph lets point insert at the end
        ;; even when the input is empty (read-only at point-max blocks it).
        (insert " ")
        (setq pi--glyph-marker (copy-marker beg t))
        (setq pi--prompt-marker (copy-marker (point) nil))))))

(defun pi--insert-transcript (text)
  "Insert TEXT (already propertized) as read-only transcript above the prompt."
  (pi--ensure-input)
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (marker-position pi--glyph-marker))
      (let ((beg (point)))
        (insert text)
        (add-text-properties beg (point) '(read-only t))))
    (when pi-auto-scroll
      (pi--scroll-to-end))))

(defun pi--append (text)
  "Append TEXT to the transcript, above the prompt input."
  (pi--insert-transcript text))

(defun pi--stream (text)
  "Append streamed TEXT to the transcript, above the prompt input."
  (pi--append text))

(defun pi--move-to-input ()
  "Move point to the editable prompt input area."
  (pi--ensure-input)
  (goto-char (point-max)))

(defun pi-newline-input ()
  "Insert a newline in the prompt input area (for multiline prompts)."
  (interactive)
  (pi--ensure-input)
  (when (< (point) (marker-position pi--prompt-marker))
    (goto-char (point-max)))
  (insert "\n"))

(defun pi--diag (fmt &rest args)
  "Write a diagnostic message to the session stderr buffer."
  (let ((msg (apply #'format fmt args)))
    (with-current-buffer (if (and (buffer-live-p pi--stderr-buffer)
                                  pi--stderr-buffer)
                             pi--stderr-buffer
                           (get-buffer-create "*pi diagnostics*"))
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert msg "\n")))))

;;; ---------------------------------------------------------------------------
;;; Content helpers
;;; ---------------------------------------------------------------------------

(defun pi--as-list (x)
  "Coerce X to a list, converting a vector (JSON array) to a list."
  (if (vectorp x) (append x nil) x))

(defun pi--render-assistant (content)
  "Render the authoritative assistant CONTENT block."
  (let ((x (pi--as-list content)))
    (cond
     ((stringp x)
      (pi--append (propertize x 'face 'pi-assistant-face)))
     ((listp x)
      (dolist (blk x)
        (when (hash-table-p blk)
          (pcase (gethash "type" blk)
            ("text"
             (pi--append (propertize (concat (or (gethash "text" blk) "") "\n")
                                     'face 'pi-assistant-face)))
            ("thinking"
             (pi--append (propertize (concat (or (gethash "thinking" blk) "") "\n")
                                     'face 'pi-thinking-face)))
            ("tool_use"
             (pi--append (propertize (format "  ⚙ %s\n" (gethash "name" blk))
                                     'face 'pi-tool-face)))
            (_ nil)))))
     (t
      (pi--append (propertize (prin1-to-string x) 'face 'pi-assistant-face))))))

;;; ---------------------------------------------------------------------------
;;; RPC transport
;;; ---------------------------------------------------------------------------

(defun pi--command (type &optional plist handler)
  "Send RPC command TYPE with PLIST and optional HANDLER.
HANDLER is called with (DATA COMMAND OK) on the matching response."
  (let* ((proc pi--process)
         (reqid (format "e-%d" (cl-incf pi--request-counter)))
         (obj (append (list :id reqid :type (symbol-name type)) plist)))
    (unless (and proc (process-live-p proc))
      (user-error "pi process is not running"))
    (when handler
      (push (cons reqid handler) pi--pending))
    (process-send-string proc (concat (json-serialize obj) "\n"))
    reqid))

(defun pi--process-filter (proc string)
  "Filter function for the pi RPC PROCESS receiving STRING."
  (when (buffer-live-p (process-buffer proc))
    (let ((acc (concat (process-get proc 'pi--stdout-accum) string)))
      (process-put proc 'pi--stdout-accum
                   (car (last (split-string acc "\n"))))
      (with-current-buffer (process-buffer proc)
        (dolist (line (butlast (split-string acc "\n")))
          (pi--handle-line line))))))

(defun pi--process-sentinel (proc string)
  "Sentinel for the pi RPC PROCESS when it exits."
  (when (memq (process-status proc) '(exit signal))
    (let ((buf (process-buffer proc)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (setq-local pi--status "exited")
          (pi--append (propertize (format "\n[pi exited: %s]\n" string)
                                  'face 'pi-error-face)))))))

(defun pi--handle-line (line)
  "Parse one JSONL LINE and dispatch it."
  (let* ((clean (string-remove-suffix "\r" line))
         (rec (condition-case err
                  (json-parse-string clean :false-object nil :null-object nil)
                (error
                 (pi--diag "JSON parse error: %S (%S)" err
                           (substring clean 0 (min 120 (length clean))))
                 nil))))
    (when rec
      (condition-case err
          (pi--handle-record rec)
        (error
         (pi--diag "Error handling record: %S\n  %s" err clean))))))

;;; ---------------------------------------------------------------------------
;;; Record dispatch
;;; ---------------------------------------------------------------------------

(defun pi--handle-record (rec)
  "Dispatch a decoded RPC record REC."
  (let ((type (gethash "type" rec)))
    (pcase type
      ("response" (pi--handle-response rec))
      ("message_start" (pi--on-message-start rec))
      ("message_update" (pi--on-message-update rec))
      ("message_end" (pi--on-message-end rec))
      ("agent_start" (setq pi--status "working"))
      ("agent_end" (setq pi--status "settling"))
      ("agent_settled" (setq pi--status "idle"))
      ("tool_execution_start"
       (setq pi--status (format "tool: %s" (gethash "toolName" rec)))
       (pi--stream (propertize (format "  ⚙ %s\n" (gethash "toolName" rec))
                               'face 'pi-tool-face)))
      ("tool_execution_update" (ignore))
      ("tool_execution_end"
       (setq pi--status "working")
       (when (not (gethash "isError" rec))
         (pi--stream (propertize "  ✓\n" 'face 'pi-tool-face))))
      ("compaction_start" (setq pi--status "compacting"))
      ("compaction_end" (setq pi--status "idle"))
      ("auto_retry_start" (setq pi--status (format "retry %s" (gethash "attempt" rec))))
      ("auto_retry_end" (setq pi--status "idle"))
      ("bash_execution_update"
       (pi--stream (propertize (or (gethash "delta" rec) "") 'face 'pi-tool-face)))
      ("session_info_changed" (setq pi--display-name (gethash "name" rec)))
      ("extension_error"
       (pi--append (propertize (format "\n[extension error] %s\n"
                                       (gethash "error" rec))
                               'face 'pi-error-face)))
      (_ nil))
    (force-mode-line-update)))

(defun pi--handle-response (rec)
  "Handle an RPC response record REC."
  (let* ((id (gethash "id" rec))
         (cmd (gethash "command" rec))
         (ok (gethash "success" rec))
         (data (gethash "data" rec))
         (handler (assoc id pi--pending)))
    (when handler
      (setq pi--pending (delq handler pi--pending))
      (funcall (cdr handler) data cmd ok))
    (when (and (not ok) (gethash "error" rec))
      (pi--append (propertize (format "\n[error] %s\n" (gethash "error" rec))
                              'face 'pi-error-face)))))

;;; ---------------------------------------------------------------------------
;;; Message handlers
;;; ---------------------------------------------------------------------------

(defun pi--on-message-start (rec)
  "Handle a message start record REC."
  (let* ((msg (gethash "message" rec))
         (role (and (hash-table-p msg) (gethash "role" msg))))
    ;; User prompts are rendered by `pi-send-prompt' at send time; the user
    ;; `message_start' event is ignored so prompts are not shown twice.
    (when (equal role "assistant")
      (pi--ensure-input)
      (setq pi--turn-start (copy-marker (marker-position pi--glyph-marker) nil)))))

(defun pi--on-message-update (rec)
  "Handle a streaming message update record REC."
  (let ((ame (gethash "assistantMessageEvent" rec)))
    (when ame
      (pcase (gethash "type" ame)
        ("text_delta"
         (pi--stream (propertize (or (gethash "delta" ame) "")
                                 'face 'pi-assistant-face)))
        ("thinking_delta"
         (pi--stream (propertize (or (gethash "delta" ame) "")
                                 'face 'pi-thinking-face)))
        (_ nil)))))

(defun pi--on-message-end (rec)
  "Handle a message end record REC (assistant messages become authoritative)."
  (let* ((msg (gethash "message" rec))
         (role (and (hash-table-p msg) (gethash "role" msg)))
         (content (and (hash-table-p msg) (gethash "content" msg))))
    (when (equal role "assistant")
      (pi--ensure-input)
      (let* ((inhibit-read-only t)
             (beg (and pi--turn-start (marker-position pi--turn-start)))
             (end (marker-position pi--glyph-marker)))
        (when beg
          (delete-region beg end)
          (pi--render-assistant content))
        (when (equal (gethash "stopReason" msg) "error")
          (pi--append (propertize (format "[model error] %s\n"
                                          (gethash "errorMessage" msg))
                                  'face 'pi-error-face)))
        (setq pi--turn-start nil)))))

;;; ---------------------------------------------------------------------------
;;; Response handlers
;;; ---------------------------------------------------------------------------

(defun pi--handle-state (data _cmd _ok)
  "Update session state from a get_state response DATA."
  (when (hash-table-p data)
    (setq pi--session-id (gethash "sessionId" data))
    (let ((m (gethash "model" data)))
      (setq pi--model-name (if (hash-table-p m) (gethash "name" m) nil)))
    (setq pi--display-name (gethash "sessionName" data))
    (force-mode-line-update)))

;;; ---------------------------------------------------------------------------
;;; Commands
;;; ---------------------------------------------------------------------------

(defun pi--find-executable ()
  "Resolve the pi CLI path, falling back to common install directories.

`pi-start' runs pi as a subprocess, so `pi-executable' must be on
`exec-path'.  If it is a bare name that `executable-find' cannot locate -
common when the nvm node bin directory is not on a GUI Emacs's `exec-path'
- scan a few standard locations for the executable and return its absolute
path."
  (or (executable-find pi-executable)
      (let* ((name (file-name-nondirectory pi-executable))
             (nvm-bin
              (when (file-directory-p "~/.nvm/versions/node")
                (mapcar (lambda (d) (expand-file-name "bin" d))
                        (cl-remove-if-not #'file-directory-p
                                          (directory-files "~/.nvm/versions/node" t)))))
             (dirs (append nvm-bin
                           '("~/.local/bin" "~/.npm-global/bin"
                             "/usr/local/bin" "/usr/bin"))))
        (cl-some (lambda (dir)
                   (let ((cand (expand-file-name name dir)))
                     (and (file-executable-p cand)
                          (not (file-directory-p cand))
                          cand)))
                 dirs))))

(defun pi--spawn-command (exe bin-dir)
  "Return a shell command that prepends BIN-DIR to PATH and execs EXE.

pi is a Node CLI (`#!/usr/bin/env node'); the Node bin that ships it is
often absent from a GUI Emacs session's PATH.  `make-process' ignores
`:environment' in this Emacs, and a child process inherits the parent's
PATH, so prepend BIN-DIR (where `node' lives) and `exec' pi in a shell."
  (let ((bin (directory-file-name bin-dir)))
    (format "export PATH=%s$PATH; exec %s"
            (shell-quote-argument (concat bin path-separator))
            (mapconcat #'shell-quote-argument (cons exe pi-rpc-args) " "))))

(defun pi-start (&optional directory)
  "Start a pi RPC agent working in DIRECTORY.
DIRECTORY defaults to `default-directory'."
  (interactive (list (read-directory-name "Pi directory: " nil default-directory t)))
  (when (null directory)
    (setq directory default-directory))
  (let* ((exe (or (pi--find-executable)
                  (user-error "pi executable not found: %s.  Set `pi-executable' to the absolute path if pi is not on `exec-path'."
                              pi-executable)))
         (bin-dir (file-name-directory exe))
         (buf (generate-new-buffer (format "*%s*" pi-buffer-name)))
         (stderr (generate-new-buffer (format "*%s diagnostics*" pi-buffer-name))))
    ;; make-process inherits the process buffer's `default-directory', so set
    ;; it before spawning (avoids the byte-compile `:cwd' keyword warning).
    (with-current-buffer buf
      (setq default-directory directory))
    (let ((proc (make-process
                 :name (buffer-name buf)
                 :buffer buf
                 :command (list "sh" "-c" (pi--spawn-command exe bin-dir))
                 :stderr stderr
                 :noquery t
                 :filter #'pi--process-filter
                 :sentinel #'pi--process-sentinel)))
      (with-current-buffer buf
        (pi-mode)
        (setq-local pi--process proc)
        (setq-local pi--stderr-buffer stderr)
        (setq-local default-directory directory)
        (setq-local pi--status "starting")
        (pi--command 'get_state nil #'pi--handle-state)
        (pi--append (propertize (format "[pi session in %s]\n" directory)
                                'face 'pi-tool-face))))
    (setq pi--last-session buf)
    (with-current-buffer buf (pi--move-to-input))
    (pop-to-buffer buf)))

(defun pi--submit-prompt (text)
  "Render the user prompt TEXT and send it to pi."
  (with-current-buffer (pi--session-buffer)
    (pi--append (concat "\n"
                        (propertize (format "> %s\n" text) 'face 'pi-user-face)
                        "\n"))
    (setq pi--status "sending")
    (pi--command 'prompt (list :message text)
                 (lambda (data _cmd ok)
                   (if ok
                       (pcase (and (hash-table-p data) (gethash "disposition" data))
                         ("started" (setq pi--status "working"))
                         ("queued" (setq pi--status "queued"))
                         (_ (setq pi--status "active")))
                     (setq pi--status "error")))))
  (pi--move-to-input))

(defun pi-send-prompt (prompt)
  "Send a PROMPT to the pi agent, read from the minibuffer."
  (interactive (list (read-from-minibuffer "Pi: " nil nil nil 'pi-prompt-history)))
  (let ((clean (string-trim prompt)))
    (when (string-empty-p clean)
      (user-error "Empty prompt"))
    (pi--submit-prompt clean)))

(defun pi-send ()
  "Send the prompt typed at the bottom of the pi session buffer."
  (interactive)
  (with-current-buffer (pi--session-buffer)
    (pi--ensure-input)
    (let* ((beg (marker-position pi--prompt-marker))
           (text (string-trim (buffer-substring beg (point-max)))))
      (let ((inhibit-read-only t))
        (delete-region beg (point-max)))
      (if (string-empty-p text)
          (user-error "Empty prompt")
        (pi--submit-prompt text)))))

(defun pi-abort ()
  "Abort the current pi run (clears the queue first)."
  (interactive)
  (with-current-buffer (pi--session-buffer)
    (pi--append (propertize "\n[abort]\n" 'face 'pi-tool-face))
    (pi--command 'clear_queue nil nil)
    (pi--command 'abort nil nil)))

(defun pi-new-session ()
  "Start a fresh pi session."
  (interactive)
  (with-current-buffer (pi--session-buffer)
    (pi--append (propertize "\n[new session]\n" 'face 'pi-tool-face))
    (pi--command 'new_session nil #'pi--handle-state)))

(defun pi-compact ()
  "Toggle manual context compaction."
  (interactive)
  (with-current-buffer (pi--session-buffer)
    (pi--append (propertize "\n[compact]\n" 'face 'pi-tool-face))
    (pi--command 'compact nil nil)))

(defun pi-cycle-model ()
  "Cycle to the next available model."
  (interactive)
  (with-current-buffer (pi--session-buffer)
    (pi--command 'cycle_model nil
                 (lambda (data _cmd _ok)
                   (let ((m (and (hash-table-p data) (gethash "model" data))))
                     (when (hash-table-p m)
                       (setq pi--model-name (gethash "name" m))))
                   (force-mode-line-update)))))

(defun pi--request (type &optional plist)
  "Send RPC command TYPE with PLIST, wait for the response and return its data.
Must be called in a pi session buffer."
  (let ((proc pi--process)
        (done nil) ok data)
    (pi--command type plist
                 (lambda (d _cmd o) (setq data d ok o done t)))
    (with-timeout (10 (user-error "pi: no response to %s" type))
      (while (not done)
        (accept-process-output proc 0.1)))
    (unless ok
      (user-error "pi: %s failed" type))
    data))

(defun pi--command-capf ()
  "Complete a /command typed at the start of the prompt input."
  (when (and pi--prompt-marker pi--process (process-live-p pi--process))
    (let ((beg (marker-position pi--prompt-marker)))
      (when (and (>= (point) beg)
                 (string-match-p "\\`/[^ \t\n]*\\'"
                                 (buffer-substring-no-properties beg (point))))
        (unless pi--commands
          (setq pi--commands
                (mapcar (lambda (c)
                          (cons (gethash "name" c) (gethash "description" c)))
                        (pi--as-list
                         (gethash "commands" (pi--request 'get_commands))))))
        (list (1+ beg) (point) pi--commands
              :exclusive 'no
              :annotation-function
              (lambda (name)
                (let ((desc (cdr (assoc name pi--commands))))
                  (and desc (concat "  " desc)))))))))

(defun pi-select-model ()
  "Pick the active model from the models pi has configured."
  (interactive)
  (with-current-buffer (pi--session-buffer)
    (let* ((models (pi--as-list
                    (gethash "models" (pi--request 'get_available_models))))
           (cands (mapcar (lambda (m)
                            (cons (format "%s/%s" (gethash "provider" m)
                                          (gethash "id" m))
                                  m))
                          models))
           (m (cdr (assoc (completing-read "Pi model: " cands nil t) cands))))
      (pi--request 'set_model (list :provider (gethash "provider" m)
                                    :modelId (gethash "id" m)))
      (setq pi--model-name (gethash "name" m))
      (force-mode-line-update)
      (message "pi model: %s" pi--model-name))))

(defun pi-set-thinking-level ()
  "Pick the thinking level from those the current model supports."
  (interactive)
  (with-current-buffer (pi--session-buffer)
    (let* ((levels (pi--as-list
                    (gethash "levels"
                             (pi--request 'get_available_thinking_levels))))
           (level (completing-read "Pi thinking level: " levels nil t)))
      (pi--request 'set_thinking_level (list :level level))
      (message "pi thinking level: %s" level))))

(defun pi-bash (command)
  "Run a shell COMMAND through pi (`bash' RPC command)."
  (interactive (list (read-string "Shell command: ")))
  (with-current-buffer (pi--session-buffer)
    (pi--append (propertize (format "\n$ %s\n" command) 'face 'pi-user-face))
    (pi--command 'bash (list :command command) nil)))

(defun pi-refresh ()
  "Refresh session state from pi."
  (interactive)
  (with-current-buffer (pi--session-buffer)
    (pi--command 'get_state nil #'pi--handle-state)))

(defun pi-switch-to-buffer ()
  "Switch to another pi session buffer."
  (interactive)
  (let ((bufs (cl-remove-if-not
               (lambda (b) (and (buffer-live-p b)
                                (buffer-local-value 'pi--process b)))
               (buffer-list))))
    (unless bufs
      (user-error "No pi sessions"))
    (let* ((target (if (cdr bufs)
                       (completing-read "Pi session: " (mapcar #'buffer-name bufs)
                                        nil t)
                     (buffer-name (car bufs)))))
      (setq pi--last-session (get-buffer target))
      (pop-to-buffer target))))

(defun pi-quit ()
  "Stop the current pi session and kill its buffers."
  (interactive)
  (with-current-buffer (pi--session-buffer)
    (when (and pi--process (process-live-p pi--process))
      (process-send-eof pi--process))
    (let ((stderr pi--stderr-buffer))
      (when (buffer-live-p stderr)
        (kill-buffer stderr)))
    (when (buffer-live-p (current-buffer))
      (kill-buffer (current-buffer)))))

(provide 'pi)

;;; funcs.el ends here
