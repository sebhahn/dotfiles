(defconst my-fabric-patterns-directory
  (expand-file-name "fabric/patterns"
                     (or (getenv "XDG_CONFIG_HOME") "~/.config")))

(defvar my-fabric-model nil
  "Fabric model to pass via -m. When nil, fabric uses its own default.")

(defvar my-fabric--model-history nil
  "History of fabric models selected via `my-fabric/select-model'.")

(defun my-fabric--list-models ()
  "Return (DEFAULT . MODELS) parsed from `fabric --listmodels'.
DEFAULT is the model marked with `*' in the listing, or nil."
  (let ((default nil)
        (models nil))
    (with-temp-buffer
      (call-process "fabric" nil t nil "--listmodels")
      (goto-char (point-min))
      (while (re-search-forward
              "^\\s-*\\(\\*\\)?\\s-*\\[[0-9]+\\][^|\n]+|\\(\\S-+\\)\\s-*$"
              nil t)
        (let ((is-default (match-string 1))
              (model (match-string 2)))
          (push model models)
          (when is-default (setq default model)))))
    (cons default (nreverse models))))

(defun my-fabric/select-model ()
  "Choose which fabric model to use. Preselects current or fabric default."
  (interactive)
  (let* ((info (my-fabric--list-models))
         (default (car info))
         (models (cdr info))
         (initial (or my-fabric-model default))
         (choice (minibuffer-with-setup-hook
                     (lambda ()
                       (when (boundp 'vertico-sort-function)
                         (setq-local vertico-sort-function
                                     #'vertico-sort-history-alpha)))
                   (completing-read
                    (format "Fabric model (default %s): " (or initial "?"))
                    models nil t nil 'my-fabric--model-history initial))))
    (setq my-fabric-model choice)
    (message "Fabric model: %s" choice)))

(defun my-fabric--pattern-list ()
  "Return the list of available fabric pattern names."
  (when (file-directory-p my-fabric-patterns-directory)
    (directory-files my-fabric-patterns-directory nil "^[^.]")))

(defvar my-fabric--pattern-history nil
  "History of fabric patterns selected via `my-fabric--read-pattern'.")

(defun my-fabric--read-pattern ()
  "Prompt for a fabric pattern name with completion, most recently used first."
  (minibuffer-with-setup-hook
      (lambda ()
        (when (boundp 'vertico-sort-function)
          (setq-local vertico-sort-function #'vertico-sort-history-alpha)))
    (completing-read "Fabric pattern: " (my-fabric--pattern-list)
                     nil t nil 'my-fabric--pattern-history)))

(defvar my-fabric--process nil
  "Currently running fabric process, if any.")

(defun my-fabric--run (input pattern)
  "Run fabric PATTERN asynchronously on INPUT (a string).
Streams output into *fabric-output*, shown in a split below."
  (when (and my-fabric--process (process-live-p my-fabric--process))
    (user-error "A fabric process is already running; use `my-fabric/kill' to stop it"))
  (let* ((output-buffer (get-buffer-create "*fabric-output*"))
         (args (append (list "--disable-responses-api" "-r" "-p" pattern)
                       (when my-fabric-model (list "-m" my-fabric-model)))))
    (with-current-buffer output-buffer
      (let ((inhibit-read-only t))
        (erase-buffer))
      (when (fboundp 'markdown-mode)
        (markdown-mode))
      (insert (format "Running fabric (pattern=%s%s)…\n\n"
                      pattern
                      (if my-fabric-model
                          (format ", model=%s" my-fabric-model) "")))
      (setq mode-line-process " [fabric]"))
    (display-buffer output-buffer
                    '((display-buffer-below-selected)
                      (window-height . 0.4)))
    (setq my-fabric--process
          (make-process
           :name "fabric"
           :buffer output-buffer
           :command (cons "fabric" args)
           :connection-type 'pipe
           :noquery t
           :sentinel
           (lambda (proc event)
             (when (memq (process-status proc) '(exit signal))
               (when (buffer-live-p (process-buffer proc))
                 (with-current-buffer (process-buffer proc)
                   (setq mode-line-process nil)
                   (goto-char (point-max))
                   (let ((inhibit-read-only t))
                     (insert (format "\n\n[fabric %s]" (string-trim event))))))
               (setq my-fabric--process nil)))))
    (process-send-string my-fabric--process input)
    (process-send-eof my-fabric--process)))

(defun my-fabric/kill ()
  "Kill the currently running fabric process, if any."
  (interactive)
  (if (and my-fabric--process (process-live-p my-fabric--process))
      (progn (kill-process my-fabric--process)
             (message "Fabric process killed."))
    (message "No fabric process running.")))

(defun my-fabric/run-on-region (start end pattern)
  "Run a fabric PATTERN on the region from START to END."
  (interactive (list (region-beginning) (region-end) (my-fabric--read-pattern)))
  (my-fabric--run (buffer-substring-no-properties start end) pattern))

(defun my-fabric/run-on-buffer (pattern)
  "Run a fabric PATTERN on the whole buffer."
  (interactive (list (my-fabric--read-pattern)))
  (my-fabric--run (buffer-substring-no-properties (point-min) (point-max))
                  pattern))

(defun my-fabric--read-file-as-text (file)
  "Return FILE's contents as text, extracting from PDF when applicable."
  (cond
   ((string-match-p "\\.pdf\\'" file)
    (unless (executable-find "pdftotext")
      (user-error "pdftotext not found; install poppler-utils to handle PDFs"))
    (with-temp-buffer
      (unless (zerop (call-process "pdftotext" nil t nil
                                   (expand-file-name file) "-"))
        (user-error "pdftotext failed on %s" file))
      (buffer-string)))
   (t
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string)))))

(defun my-fabric/run-on-file (file pattern)
  "Run a fabric PATTERN on the contents of FILE.
PDFs are extracted via pdftotext; other files are read as text."
  (interactive (list (read-file-name "Fabric on file: " nil nil t)
                     (my-fabric--read-pattern)))
  (my-fabric--run (my-fabric--read-file-as-text file) pattern))
