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

(defcustom pi-rpc-args '("--mode" "rpc")
  "Arguments passed to pi when starting an RPC session.

Sessions are saved so they can be resumed (`pi-resume-session').  Add
`--no-session' for ephemeral sessions.  Other common additions: `--name
NAME', `--provider NAME', `--model ID', `--session-dir PATH'.  See
`pi --help' for the installed version."
  :type '(repeat string)
  :group 'pi)

(defcustom pi-session-directory "~/.pi/agent/sessions/"
  "Directory where pi stores session files (used by `pi-resume-session')."
  :type 'directory
  :group 'pi)

(defcustom pi-busy-send-behavior "steer"
  "How a prompt sent while pi is working is delivered.
\"steer\" delivers it after the current tool calls, before the next model
call (like Enter in pi's TUI).  \"followUp\" waits until the whole task is
done (like Alt+Enter).  `pi-send-follow-up' always uses \"followUp\"."
  :type '(choice (const "steer") (const "followUp"))
  :group 'pi)

(defcustom pi-fontify-markdown t
  "If non-nil, highlight Markdown in finished replies (needs `markdown-mode')."
  :type 'boolean
  :group 'pi)

(defcustom pi-show-thinking nil
  "If non-nil, show the model's thinking text in new session buffers.
Toggle it per buffer with `pi-toggle-thinking'."
  :type 'boolean
  :group 'pi)

(defcustom pi-tool-output-max-lines 200
  "Maximum number of tool output lines kept in the transcript."
  :type 'integer
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
(defvar pi--busy nil
  "Non-nil while pi is running (between agent_start and agent_settled).")
(defvar pi--thinking-level nil)
(defvar pi--stats nil
  "Formatted context/cost summary for the header line.")
(defvar pi--ext-status nil
  "Extension status entries for the header line, as (KEY . TEXT).")
(defvar pi--tools nil
  "Hash table of tool calls by id, each a plist of markers.")
(defvar pi--pending-images nil
  "Images attached to the next prompt (`pi-attach-image').")
(defvar pi--last-session nil
  "Most recently used pi session buffer (global).")
(defvar pi--busy-since nil
  "`float-time' when the current run started, for the elapsed time display.")
(defvar pi--last-error nil
  "Model error of the latest attempt; printed once pi stops retrying.")
(defvar pi--history-index nil
  "Position in `pi-prompt-history' while browsing with M-p/M-n.")
(defvar pi--ticker nil
  "Timer refreshing the header of busy pi buffers every second.")
(defvar pi-prompt-history nil
  "Prompts sent to pi, newest first (shared by all sessions).")
(defvar pi-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Commands use non-letter keys so typing a prompt is unaffected.
    (define-key map [return] #'pi-send)
    (define-key map (kbd "C-<return>") #'pi-send)
    (define-key map (kbd "C-j") #'pi-newline-input)
    (define-key map (kbd "M-p") #'pi-previous-input)
    (define-key map (kbd "M-n") #'pi-next-input)
    (define-key map (kbd "TAB") #'completion-at-point)
    (define-key map (kbd "C-c C-c") #'pi-send)
    (define-key map (kbd "C-c C-f") #'pi-send-follow-up)
    (define-key map (kbd "C-c C-a") #'pi-abort)
    (define-key map (kbd "C-c C-n") #'pi-new-session)
    (define-key map (kbd "C-c C-m") #'pi-select-model)
    (define-key map (kbd "C-c C-t") #'pi-set-thinking-level)
    (define-key map (kbd "C-c C-h") #'pi-toggle-thinking)
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
multiline prompts.  While pi is working, RET steers and `C-c C-f' queues
a follow-up.  RET or TAB on a tool line shows or hides its output.
Other session commands are on the SPC leader (`SPC $ p ...'), the major
mode leader (`,') or via `C-c C-<key>' (a abort, n new session,
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
  (setq-local pi--busy nil)
  (setq-local pi--busy-since nil)
  (setq-local pi--last-error nil)
  (setq-local pi--history-index nil)
  (setq-local pi--thinking-level nil)
  (setq-local pi--stats nil)
  (setq-local pi--ext-status nil)
  (setq-local pi--tools (make-hash-table :test #'equal))
  (setq-local pi--pending-images nil)
  (unless pi-show-thinking
    (add-to-invisibility-spec 'pi-thinking))
  (add-hook 'completion-at-point-functions #'pi--command-capf nil t)
  (setq-local header-line-format '(:eval (pi--header-line))))

;; Start pi buffers in Evil insert state so the prompt is typeable and ESC
;; switches to normal state.
(with-eval-after-load 'evil
  (add-to-list 'evil-insert-state-modes 'pi-mode))

(defun pi--header-line ()
  "Return the header line for a pi session buffer."
  (mapconcat
   #'identity
   (delq nil
         (list (format " π %s%s" (or pi--model-name "no model")
                       (if (member pi--thinking-level '(nil "off")) ""
                         (format " [%s]" pi--thinking-level)))
               (if pi--busy-since
                   (format "%s %ds" pi--status
                           (round (- (float-time) pi--busy-since)))
                 pi--status)
               pi--stats
               pi--display-name
               (and pi--pending-images
                    (format "%d image(s) attached" (length pi--pending-images)))
               (and pi--ext-status
                    (mapconcat #'cdr pi--ext-status " "))))
   " · "))

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

(defun pi--content-text (content)
  "Return the text of CONTENT (a string or a list of content blocks)."
  (let ((x (pi--as-list content)))
    (if (stringp x)
        x
      (mapconcat (lambda (blk)
                   (pcase (and (hash-table-p blk) (gethash "type" blk))
                     ("text" (or (gethash "text" blk) ""))
                     ("image" "[image]")
                     (_ "")))
                 x ""))))

(declare-function gfm-mode "markdown-mode")

(defun pi--fontify-markdown (text)
  "Return TEXT highlighted as Markdown, keeping only face properties."
  (if (not (and pi-fontify-markdown (require 'markdown-mode nil t)))
      (propertize text 'face 'pi-assistant-face)
    (with-temp-buffer
      (insert text)
      ;; No mode hooks: avoid spell checkers, LSP etc. for every reply.
      (delay-mode-hooks (gfm-mode))
      ;; Language modes used for code blocks can be chatty; keep them quiet.
      (let ((inhibit-message t)
            (message-log-max nil))
        (font-lock-ensure))
      (let ((s (buffer-substring-no-properties (point-min) (point-max)))
            (pos (point-min)))
        (while (< pos (point-max))
          (let ((next (next-single-property-change pos 'face nil (point-max)))
                (face (get-text-property pos 'face)))
            (when face
              (put-text-property (1- pos) (1- next) 'face face s))
            (setq pos next)))
        (add-face-text-property 0 (length s) 'pi-assistant-face t s)
        s))))

(defun pi--render-assistant (content &optional history)
  "Render the authoritative assistant CONTENT block.
With HISTORY non-nil (redrawing a loaded session), also render tool calls,
which live sessions show from tool execution events instead."
  (let ((x (pi--as-list content)))
    (cond
     ((stringp x)
      (pi--append (propertize x 'face 'pi-assistant-face)))
     ((listp x)
      (dolist (blk x)
        (when (hash-table-p blk)
          (pcase (gethash "type" blk)
            ("text"
             (pi--append (concat (pi--fontify-markdown
                                  (string-trim (or (gethash "text" blk) "")))
                                 "\n")))
            ("thinking"
             (let ((s (string-trim (or (gethash "thinking" blk) ""))))
               (unless (string-empty-p s)
                 (pi--append (propertize (concat s "\n\n")
                                         'face 'pi-thinking-face
                                         'invisible 'pi-thinking)))))
            ("toolCall"
             (when history
               (pi--tool-start (gethash "id" blk) (gethash "name" blk)
                               (gethash "arguments" blk))))
            (_ nil)))))
     (t
      (pi--append (propertize (prin1-to-string x) 'face 'pi-assistant-face))))))

;;; ---------------------------------------------------------------------------
;;; Tool calls: one header line per call, output folded below it
;;; ---------------------------------------------------------------------------

(defvar pi-tool-header-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'pi-toggle-tool-output)
    (define-key map [return] #'pi-toggle-tool-output)
    (define-key map (kbd "TAB") #'pi-toggle-tool-output)
    (define-key map [mouse-1] #'pi-toggle-tool-output)
    map)
  "Keymap active on tool header lines.")

(defun pi--tool-args-summary (args)
  "Return a one-line summary of tool ARGS."
  (when (hash-table-p args)
    (let ((s (or (cl-some (lambda (k)
                            (let ((v (gethash k args)))
                              (and (stringp v) v)))
                          '("command" "path" "file_path" "pattern" "query" "url"))
                 (json-serialize args))))
      (truncate-string-to-width
       (replace-regexp-in-string "\n" " ⏎ " s) 100 nil nil "…"))))

(defun pi--tool-start (id name args)
  "Insert a header line for tool call ID running NAME with ARGS."
  (let ((summary (pi--tool-args-summary args))
        hdr hdr-end)
    (pi--ensure-input)
    (setq hdr (copy-marker (marker-position pi--glyph-marker) nil))
    (pi--append (propertize (format "  ⚙ %s%s" name
                                    (if summary (concat ": " summary) ""))
                            'face 'pi-tool-face))
    (setq hdr-end (copy-marker (marker-position pi--glyph-marker) nil))
    (pi--append "\n")
    (puthash id (list :hdr hdr :hdr-end hdr-end
                      :body (copy-marker (marker-position pi--glyph-marker) nil))
             pi--tools)))

(defun pi--tool-end (id content is-error)
  "Finish tool call ID: mark its status and insert CONTENT folded below it."
  (let ((tool (gethash id pi--tools)))
    (unless tool
      (pi--tool-start id "tool" nil)
      (setq tool (gethash id pi--tools)))
    (remhash id pi--tools)
    (let* ((inhibit-read-only t)
           (lines (split-string (string-trim-right (or (pi--content-text content) ""))
                                "\n"))
           (lines (if (equal lines '("")) nil lines))
           (n (length lines))
           (shown (seq-take lines pi-tool-output-max-lines))
           (hdr (plist-get tool :hdr))
           (hdr-end (plist-get tool :hdr-end))
           (body (plist-get tool :body)))
      ;; Status after the header text.
      (save-excursion
        (goto-char hdr-end)
        (insert (propertize (format "  %s%s" (if is-error "✗" "✓")
                                    (if (> n 0) (format " (%d lines)" n) ""))
                            'face (if is-error 'pi-error-face 'pi-tool-face)
                            'read-only t)))
      (when shown
        (save-excursion
          (goto-char body)
          (let ((beg (point)))
            (insert (propertize
                     (concat (mapconcat (lambda (l) (concat "    " l)) shown "\n")
                             (if (> n (length shown))
                                 (format "\n    … %d more lines" (- n (length shown)))
                               "")
                             "\n")
                     'face 'pi-tool-face 'read-only t))
            (let ((ov (make-overlay beg (point))))
              (overlay-put ov 'evaporate t)
              ;; Errors stay visible; normal output starts folded.
              (overlay-put ov 'invisible (not is-error))
              (add-text-properties
               hdr (save-excursion (goto-char hdr) (line-end-position))
               (list 'pi-tool-overlay ov
                     'keymap pi-tool-header-map
                     'mouse-face 'highlight
                     'help-echo "RET/TAB/mouse-1: show or hide output")))))))))

(defun pi-toggle-thinking ()
  "Show or hide the model's thinking text in the pi session buffer."
  (interactive)
  (with-current-buffer (pi--session-buffer)
    (if (memq 'pi-thinking (if (listp buffer-invisibility-spec)
                               buffer-invisibility-spec))
        (progn (remove-from-invisibility-spec 'pi-thinking)
               (message "pi: thinking shown"))
      (add-to-invisibility-spec 'pi-thinking)
      (message "pi: thinking hidden"))))

(defun pi-toggle-tool-output (&optional event)
  "Show or hide the output of the tool call on this line (or clicked EVENT)."
  (interactive (list last-nonmenu-event))
  (when (mouse-event-p event)
    (mouse-set-point event))
  (let ((ov (get-text-property (point) 'pi-tool-overlay)))
    (unless ov
      (user-error "No tool output here"))
    (overlay-put ov 'invisible (not (overlay-get ov 'invisible)))))

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
    (pi--send-raw obj)
    reqid))

(defun pi--send-raw (obj)
  "Write OBJ (a plist) as one JSON line to the pi process."
  (unless (and pi--process (process-live-p pi--process))
    (user-error "pi process is not running"))
  (process-send-string pi--process (concat (json-serialize obj) "\n")))

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
          (setq-local pi--busy nil)
          (setq-local pi--busy-since nil)
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
      ("agent_start"
       (setq pi--busy t pi--status "working")
       (unless pi--busy-since (setq pi--busy-since (float-time)))
       (pi--ensure-ticker))
      ("agent_end" (setq pi--status "settling"))
      ("agent_settled"
       (setq pi--busy nil pi--busy-since nil pi--status "idle")
       (when pi--last-error
         (pi--append (propertize (format "[model error] %s\n" pi--last-error)
                                 'face 'pi-error-face))
         (setq pi--last-error nil))
       (pi--refresh-stats))
      ("tool_execution_start"
       (setq pi--status (format "tool: %s" (gethash "toolName" rec)))
       (pi--tool-start (gethash "toolCallId" rec) (gethash "toolName" rec)
                       (gethash "args" rec)))
      ("tool_execution_update" (ignore))
      ("tool_execution_end"
       (setq pi--status "working")
       (let ((result (gethash "result" rec)))
         (pi--tool-end (gethash "toolCallId" rec)
                       (and (hash-table-p result) (gethash "content" result))
                       (gethash "isError" rec))))
      ("compaction_start" (setq pi--status "compacting"))
      ("compaction_end"
       (setq pi--status (if pi--busy "working" "idle"))
       (pi--refresh-stats))
      ("thinking_level_changed" (setq pi--thinking-level (gethash "level" rec)))
      ("extension_ui_request" (pi--on-ui-request rec))
      ("auto_retry_start"
       (setq pi--status (format "retry %s/%s" (gethash "attempt" rec)
                                (gethash "maxAttempts" rec))))
      ("auto_retry_end" (setq pi--status (if pi--busy "working" "idle")))
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
                                 'face 'pi-thinking-face
                                 'invisible 'pi-thinking)))
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
        ;; pi may retry a failed call; print the error once it settles.
        (setq pi--last-error (and (equal (gethash "stopReason" msg) "error")
                                  (gethash "errorMessage" msg)))
        (setq pi--turn-start nil)))))

(defun pi--tick ()
  "Refresh the header of busy pi buffers; stop when none is busy."
  (let ((any nil))
    (dolist (b (buffer-list))
      (when (and (eq (buffer-local-value 'major-mode b) 'pi-mode)
                 (buffer-local-value 'pi--busy b))
        (setq any t)
        (with-current-buffer b (force-mode-line-update))))
    (unless any
      (cancel-timer pi--ticker)
      (setq pi--ticker nil))))

(defun pi--ensure-ticker ()
  "Start the header refresh timer if it is not running."
  (unless pi--ticker
    (setq pi--ticker (run-at-time 1 1 #'pi--tick))))

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
    (setq pi--thinking-level (gethash "thinkingLevel" data))
    (when (equal pi--status "starting")
      (setq pi--status "idle"))
    (force-mode-line-update)))

(defun pi--format-tokens (n)
  "Format token count N compactly, e.g. 31k."
  (if (and (numberp n) (>= n 1000)) (format "%dk" (round n 1000)) (format "%s" n)))

(defun pi--refresh-stats ()
  "Fetch context usage and cost for the header line."
  (pi--command
   'get_session_stats nil
   (lambda (data _cmd ok)
     (when (and ok (hash-table-p data))
       (let* ((ctx (gethash "contextUsage" data))
              (cost (gethash "cost" data))
              (parts (delq nil
                           (list (and (hash-table-p ctx) (gethash "percent" ctx)
                                      (format "ctx %d%% (%s/%s)"
                                              (round (gethash "percent" ctx))
                                              (pi--format-tokens (gethash "tokens" ctx))
                                              (pi--format-tokens
                                               (gethash "contextWindow" ctx))))
                                 (and (numberp cost) (> cost 0)
                                      (format "$%.2f" cost))))))
         (setq pi--stats (and parts (string-join parts " ")))
         (force-mode-line-update))))))

;;; ---------------------------------------------------------------------------
;;; Extension UI requests (dialogs, notifications, status)
;;; ---------------------------------------------------------------------------

(defun pi--on-ui-request (rec)
  "Handle an extension_ui_request record REC."
  (pcase (gethash "method" rec)
    ((or "select" "confirm" "input" "editor")
     ;; Ask outside the process filter, where C-g works and input is safe.
     (run-at-time 0 nil #'pi--answer-ui-request (current-buffer) rec))
    ("notify"
     (let ((type (or (gethash "notifyType" rec) "info"))
           (msg (gethash "message" rec)))
       (message "pi: %s" msg)
       (unless (equal type "info")
         (pi--append (propertize (format "[%s] %s\n" type msg)
                                 'face (if (equal type "error")
                                           'pi-error-face 'pi-tool-face))))))
    ("setStatus"
     (let ((key (gethash "statusKey" rec))
           (text (gethash "statusText" rec)))
       (setq pi--ext-status (assoc-delete-all key pi--ext-status))
       (when (and text (not (string-empty-p text)))
         (setq pi--ext-status (append pi--ext-status (list (cons key text)))))))
    ("set_editor_text"
     (pi--ensure-input)
     (let ((inhibit-read-only t))
       (delete-region pi--prompt-marker (point-max))
       (save-excursion
         (goto-char (point-max))
         (insert (or (gethash "text" rec) "")))))
    (_ nil)))

(defun pi--answer-ui-request (buf rec)
  "Ask the user for extension dialog REC and send the answer from BUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let* ((title (or (gethash "title" rec) "pi"))
             (msg (gethash "message" rec))
             (reply
              (condition-case nil
                  (pcase (gethash "method" rec)
                    ("select"
                     (list :value (completing-read
                                   (format "%s: " title)
                                   (pi--as-list (gethash "options" rec)) nil t)))
                    ("confirm"
                     (list :confirmed
                           (if (y-or-n-p (if msg (format "%s - %s " title msg)
                                           (format "%s " title)))
                               t :false)))
                    ("input"
                     (list :value
                           (read-string
                            (if (gethash "placeholder" rec)
                                (format "%s (%s): " title (gethash "placeholder" rec))
                              (format "%s: " title)))))
                    ("editor"
                     (list :value (read-string (format "%s: " title)
                                               (gethash "prefill" rec)))))
                (quit (list :cancelled t)))))
        (when (and pi--process (process-live-p pi--process))
          (pi--send-raw (append (list :type "extension_ui_response"
                                      :id (gethash "id" rec))
                                reply)))))))

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
         (dir-name (file-name-nondirectory (directory-file-name directory)))
         (buf (generate-new-buffer (format "*%s: %s*" pi-buffer-name dir-name)))
         (stderr (generate-new-buffer
                  (format "*%s diagnostics: %s*" pi-buffer-name dir-name))))
    (with-current-buffer buf
      (setq default-directory directory))
    ;; make-process runs in the *current* buffer's `default-directory'.
    (let ((proc (let ((default-directory (file-name-as-directory
                                          (expand-file-name directory))))
                  (make-process
                   :name (buffer-name buf)
                   :buffer buf
                   :command (list "sh" "-c" (pi--spawn-command exe bin-dir))
                   :stderr stderr
                   :noquery t
                   :filter #'pi--process-filter
                   :sentinel #'pi--process-sentinel))))
      (with-current-buffer buf
        (pi-mode)
        (setq-local pi--process proc)
        (setq-local pi--stderr-buffer stderr)
        (setq-local default-directory directory)
        (setq-local pi--status "starting")
        (pi--command 'get_state nil #'pi--handle-state)
        (pi--refresh-stats)
        (pi--append (propertize (format "[pi session in %s]\n" directory)
                                'face 'pi-tool-face))))
    (setq pi--last-session buf)
    (with-current-buffer buf (pi--move-to-input))
    (pop-to-buffer buf)))

(defun pi--submit-prompt (text &optional behavior)
  "Render the user prompt TEXT and send it to pi.
While pi is working the prompt is queued with BEHAVIOR (\"steer\" or
\"followUp\", default `pi-busy-send-behavior'); /commands run immediately."
  (with-current-buffer (pi--session-buffer)
    (let* ((queue (and pi--busy (not (string-prefix-p "/" text))
                       (or behavior pi-busy-send-behavior)))
           (images pi--pending-images)
           (plist (append (list :message text)
                          (and queue (list :streamingBehavior queue))
                          (and images (list :images (vconcat (reverse images)))))))
      (pi--append
       (concat "\n"
               (propertize (format "> %s\n" text) 'face 'pi-user-face)
               (if (or queue images)
                   (propertize
                    (concat (if images (format "  [%d image(s)]" (length images)) "")
                            (if queue (format "  [queued: %s]" queue) "")
                            "\n")
                    'face 'pi-tool-face)
                 "")
               "\n"))
      (setq pi--pending-images nil)
      (setq pi--history-index nil)
      (add-to-history 'pi-prompt-history text)
      (setq pi--status "sending")
      (pi--command 'prompt plist
                 (lambda (data _cmd ok)
                   (if ok
                       (pcase (and (hash-table-p data) (gethash "disposition" data))
                         ("started" (setq pi--status "working"))
                         ("queued" (setq pi--status "queued"))
                         (_ (setq pi--status "active")))
                     (setq pi--status "error"))))))
  (pi--move-to-input))

(defun pi-send-prompt (prompt)
  "Send a PROMPT to the pi agent, read from the minibuffer."
  (interactive (list (read-from-minibuffer "Pi: " nil nil nil 'pi-prompt-history)))
  (let ((clean (string-trim prompt)))
    (when (string-empty-p clean)
      (user-error "Empty prompt"))
    (pi--submit-prompt clean)))

(defun pi-send (&optional behavior)
  "Send the prompt typed at the bottom of the pi session buffer.
While pi is working it is queued with BEHAVIOR (default
`pi-busy-send-behavior')."
  (interactive)
  (with-current-buffer (pi--session-buffer)
    (pi--ensure-input)
    (let* ((beg (marker-position pi--prompt-marker))
           (text (string-trim (buffer-substring-no-properties beg (point-max)))))
      (when (string-empty-p text)
        (user-error "Empty prompt"))
      (let ((inhibit-read-only t))
        (delete-region beg (point-max)))
      (pi--submit-prompt text behavior))))

(defun pi-send-follow-up ()
  "Send the bottom prompt as a follow-up, delivered when pi finishes the task."
  (interactive)
  (pi-send "followUp"))

(defun pi-abort ()
  "Abort the current pi run (clears the queue first)."
  (interactive)
  (with-current-buffer (pi--session-buffer)
    (pi--append (propertize "\n[abort]\n" 'face 'pi-tool-face))
    ;; Put queued (never delivered) prompts back into the input, like pi's TUI.
    (pi--command 'clear_queue nil
                 (lambda (data _cmd ok)
                   (let ((queued (and ok (hash-table-p data)
                                      (append (pi--as-list (gethash "steering" data))
                                              (pi--as-list (gethash "followUp" data))))))
                     (when queued
                       (pi--ensure-input)
                       (let ((current (string-trim
                                       (buffer-substring-no-properties
                                        pi--prompt-marker (point-max)))))
                         (pi--set-input
                          (string-join (append queued
                                               (unless (string-empty-p current)
                                                 (list current)))
                                       "\n\n")))
                       (message "pi: %d queued prompt(s) returned to the input"
                                (length queued))))))
    (pi--command 'abort nil nil)))

(defun pi--set-input (text)
  "Replace the prompt input with TEXT."
  (pi--ensure-input)
  (let ((inhibit-read-only t))
    (delete-region pi--prompt-marker (point-max)))
  (save-excursion
    (goto-char (point-max))
    (insert text))
  (when (eq (current-buffer) (window-buffer))
    (goto-char (point-max))))

(defun pi-previous-input ()
  "Replace the prompt input with the previous prompt from history."
  (interactive)
  (let ((i (if pi--history-index (1+ pi--history-index) 0)))
    (when (>= i (length pi-prompt-history))
      (user-error "No older prompt"))
    (setq pi--history-index i)
    (pi--set-input (nth i pi-prompt-history))))

(defun pi-next-input ()
  "Replace the prompt input with the next prompt from history."
  (interactive)
  (cond
   ((null pi--history-index) (user-error "No newer prompt"))
   ((= pi--history-index 0)
    (setq pi--history-index nil)
    (pi--set-input ""))
   (t
    (setq pi--history-index (1- pi--history-index))
    (pi--set-input (nth pi--history-index pi-prompt-history)))))

(defun pi-new-session ()
  "Start a fresh pi session."
  (interactive)
  (with-current-buffer (pi--session-buffer)
    (pi--append (propertize "\n[new session]\n" 'face 'pi-tool-face))
    (pi--command 'new_session nil
                 (lambda (&rest _)
                   (pi--command 'get_state nil #'pi--handle-state)
                   (pi--refresh-stats)))))

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
      ;; The thinking level and context window can change with the model.
      (pi--command 'get_state nil #'pi--handle-state)
      (pi--refresh-stats)
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
      (setq pi--thinking-level level)
      (force-mode-line-update)
      (message "pi thinking level: %s" level))))

;;; ---------------------------------------------------------------------------
;;; Sessions
;;; ---------------------------------------------------------------------------

(defun pi--completing-read-ordered (prompt cands)
  "Read one of CANDS (an alist) with PROMPT, keeping their order.
Return the cdr of the chosen entry."
  (let ((choice (completing-read
                 prompt
                 (lambda (str pred action)
                   (if (eq action 'metadata)
                       '(metadata (display-sort-function . identity)
                                  (cycle-sort-function . identity))
                     (complete-with-action action cands str pred)))
                 nil t)))
    (cdr (assoc choice cands))))

(defun pi--session-files (directory)
  "Return pi session files for DIRECTORY, newest first."
  (let* ((path (directory-file-name (expand-file-name directory)))
         (enc (concat "--"
                      (replace-regexp-in-string "[/\\\\:]" "-"
                                                (string-remove-prefix "/" path))
                      "--"))
         (dir (expand-file-name enc pi-session-directory)))
    (when (file-directory-p dir)
      (nreverse (directory-files dir t "\\.jsonl\\'")))))

(defun pi--session-label (file)
  "Return a label for session FILE: date plus its name or first prompt."
  (let ((name nil) (first nil))
    (with-temp-buffer
      (insert-file-contents file nil 0 262144)
      (dolist (line (split-string (buffer-string) "\n" t))
        (let ((rec (ignore-errors
                     (json-parse-string line :false-object nil :null-object nil))))
          (when (hash-table-p rec)
            (pcase (gethash "type" rec)
              ("session_info" (setq name (gethash "name" rec)))
              ("message"
               (let ((m (gethash "message" rec)))
                 (when (and (not first) (hash-table-p m)
                            (equal (gethash "role" m) "user"))
                   (setq first (pi--content-text (gethash "content" m)))))))))))
    (let ((base (file-name-nondirectory file)))
      (format "%s  %s"
              (if (string-match "\\`\\([0-9-]+\\)T\\([0-9]+\\)-\\([0-9]+\\)" base)
                  (format "%s %s:%s" (match-string 1 base) (match-string 2 base)
                          (match-string 3 base))
                base)
              (truncate-string-to-width
               (replace-regexp-in-string "[\n\t ]+" " " (or name first "(empty)"))
               80 nil nil "…")))))

(defun pi--clear-transcript ()
  "Delete the transcript above the prompt."
  (pi--ensure-input)
  (let ((inhibit-read-only t))
    (delete-region (point-min) pi--glyph-marker))
  (clrhash pi--tools)
  (setq pi--turn-start nil)
  (pi--append (propertize (format "[pi session in %s]\n" default-directory)
                          'face 'pi-tool-face)))

(defun pi--render-history-message (msg)
  "Render a stored session message MSG."
  (when (hash-table-p msg)
    (pcase (gethash "role" msg)
      ("user"
       (pi--append (concat "\n"
                           (propertize (format "> %s\n"
                                               (pi--content-text (gethash "content" msg)))
                                       'face 'pi-user-face)
                           "\n")))
      ("assistant"
       (pi--render-assistant (gethash "content" msg) t)
       (when (equal (gethash "stopReason" msg) "error")
         (pi--append (propertize (format "[model error] %s\n"
                                         (gethash "errorMessage" msg))
                                 'face 'pi-error-face))))
      ("toolResult"
       (pi--tool-end (gethash "toolCallId" msg) (gethash "content" msg)
                     (gethash "isError" msg)))
      ("bashExecution"
       (pi--append (concat (propertize (format "\n$ %s\n" (gethash "command" msg))
                                       'face 'pi-user-face)
                           (propertize (concat (or (gethash "output" msg) "") "\n")
                                       'face 'pi-tool-face))))
      ((or "compactionSummary" "branchSummary")
       (pi--append (propertize (format "[%s]\n" (gethash "role" msg))
                               'face 'pi-tool-face)))
      (_ nil))))

(defun pi--reload-transcript ()
  "Redraw the transcript from the session's messages and refresh state."
  (let ((msgs (pi--as-list (gethash "messages" (pi--request 'get_messages)))))
    (pi--clear-transcript)
    (dolist (m msgs)
      (pi--render-history-message m))
    (pi--command 'get_state nil #'pi--handle-state)
    (pi--refresh-stats)))

(defun pi-resume-session ()
  "Load a saved pi session for this directory into the current pi buffer."
  (interactive)
  (with-current-buffer (pi--session-buffer)
    (let* ((files (or (pi--session-files default-directory)
                      (user-error "No saved pi sessions for %s" default-directory)))
           (file (pi--completing-read-ordered
                  "Resume pi session: "
                  (mapcar (lambda (f) (cons (pi--session-label f) f)) files)))
           (data (pi--request 'switch_session (list :sessionPath file))))
      (if (and (hash-table-p data) (gethash "cancelled" data))
          (message "pi: session switch was cancelled")
        (pi--reload-transcript)))))

(defun pi-fork-session ()
  "Fork the session from an earlier prompt; that prompt is put back in the input."
  (interactive)
  (with-current-buffer (pi--session-buffer)
    (let* ((msgs (or (pi--as-list
                      (gethash "messages" (pi--request 'get_fork_messages)))
                     (user-error "No prompts to fork from")))
           (entry (pi--completing-read-ordered
                   "Fork from prompt: "
                   (mapcar (lambda (m)
                             (cons (truncate-string-to-width
                                    (replace-regexp-in-string
                                     "[\n\t ]+" " " (or (gethash "text" m) ""))
                                    100 nil nil "…")
                                   (gethash "entryId" m)))
                           (reverse msgs))))
           (data (pi--request 'fork (list :entryId entry))))
      (if (and (hash-table-p data) (gethash "cancelled" data))
          (message "pi: fork was cancelled")
        (pi--reload-transcript)
        (pi--move-to-input)
        (insert (or (gethash "text" data) ""))))))

(defun pi-rename-session (name)
  "Set the display NAME of the current pi session."
  (interactive
   (list (with-current-buffer (pi--session-buffer)
           (read-string "Session name: " pi--display-name))))
  (with-current-buffer (pi--session-buffer)
    (pi--request 'set_session_name (list :name name))
    (setq pi--display-name name)
    (force-mode-line-update)))

(defun pi-export-html ()
  "Export the current pi session to an HTML file."
  (interactive)
  (with-current-buffer (pi--session-buffer)
    (let ((path (expand-file-name (gethash "path" (pi--request 'export_html))
                                  default-directory)))
      (kill-new path)
      (message "pi session exported to %s (path copied)" path))))

;;; ---------------------------------------------------------------------------
;;; Sending context from other buffers
;;; ---------------------------------------------------------------------------

(defun pi--file-reference (file)
  "Return FILE relative to the pi session directory when inside it."
  (let ((root (with-current-buffer (pi--session-buffer) default-directory)))
    (if (file-in-directory-p file root)
        (file-relative-name file root)
      (abbreviate-file-name file))))

(defun pi--insert-into-input (text)
  "Insert TEXT at the end of the pi prompt input and show the session."
  (let ((buf (pi--session-buffer)))
    (with-current-buffer buf
      (pi--move-to-input)
      (insert text))
    (pop-to-buffer buf)
    (goto-char (point-max))))

(defun pi-insert-region (beg end)
  "Insert the region BEG..END, with its file and lines, into the pi prompt."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties beg end))
         (lines (format "%d-%d" (line-number-at-pos beg)
                        (line-number-at-pos (max beg (1- end)))))
         (where (if buffer-file-name
                    (format "`%s` lines %s" (pi--file-reference buffer-file-name) lines)
                  (format "buffer %s" (buffer-name))))
         (lang (replace-regexp-in-string
                "\\(-ts\\)?-mode\\'" "" (symbol-name major-mode))))
    (deactivate-mark)
    (pi--insert-into-input
     (format "From %s:\n```%s\n%s\n```\n" where lang (string-trim-right text)))))

(defun pi-insert-file (file)
  "Insert a reference to FILE into the pi prompt (pi reads it with its tools)."
  (interactive (list (read-file-name "File for pi: " nil buffer-file-name t
                                     (and buffer-file-name
                                          (file-name-nondirectory buffer-file-name)))))
  (pi--insert-into-input (format "`%s` " (pi--file-reference (expand-file-name file)))))

(defun pi-attach-image (file)
  "Attach image FILE to the next pi prompt.  With a prefix arg, clear attachments."
  (interactive
   (list (unless current-prefix-arg
           (read-file-name "Image for pi: " nil nil t nil
                           (lambda (f) (or (file-directory-p f)
                                           (string-match-p "\\.\\(png\\|jpe?g\\|gif\\|webp\\)\\'"
                                                           (downcase f))))))))
  (with-current-buffer (pi--session-buffer)
    (if (null file)
        (progn (setq pi--pending-images nil)
               (message "pi: image attachments cleared"))
      (let* ((ext (downcase (or (file-name-extension file) "")))
             (mime (pcase ext
                     ("png" "image/png") ((or "jpg" "jpeg") "image/jpeg")
                     ("gif" "image/gif") ("webp" "image/webp")
                     (_ (user-error "Unsupported image type: %s" ext))))
             (data (with-temp-buffer
                     (set-buffer-multibyte nil)
                     (insert-file-contents-literally file)
                     (base64-encode-region (point-min) (point-max) t)
                     (buffer-string))))
        (push (list :type "image" :data data :mimeType mime) pi--pending-images)
        (message "pi: attached %s (%d image(s) for the next prompt)"
                 (file-name-nondirectory file) (length pi--pending-images))))
    (force-mode-line-update)))

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
