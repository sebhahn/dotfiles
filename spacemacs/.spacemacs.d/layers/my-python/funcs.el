(defun my-python/send-region-compilation(start end)
  (interactive "r")
  (let ((cur-window (selected-window)))
    (append-to-buffer (get-buffer "*compilation*") start end)
    (switch-to-buffer-other-window (get-buffer "*compilation*"))
    (evil-insert-state)
    (execute-kbd-macro (read-kbd-macro "DEL"))
    (execute-kbd-macro (read-kbd-macro "RET"))
    (select-window cur-window)))

(defun my-python/send-region-compilation-dbg(start end)
  (interactive "r")
  (append-to-buffer (get-buffer "*compilation*") start end)
  (switch-to-buffer-other-window (get-buffer "*compilation*"))
  (evil-insert-state)
  (execute-kbd-macro (read-kbd-macro "DEL"))
  (execute-kbd-macro (read-kbd-macro "RET")))

(defvar my-python--last-cmd nil)
(defvar my-python--last-dir nil)
(defvar my-python--last-compile-start-time nil)

(defun my-python--rotate-compilation-buffer (_)
  "Rename existing *compilation* to *compilation-N* and return \"*compilation*\"."
  (when (get-buffer "*compilation*")
    (let ((n 2))
      (while (get-buffer (format "*compilation-%d*" n))
        (cl-incf n))
      (with-current-buffer (get-buffer "*compilation*")
        (rename-buffer (format "*compilation-%d*" n)))))
  "*compilation*")

(defun my-python--run-cmd (cmd)
  (let ((start-time (float-time))
        (buf (compilation-start cmd t #'my-python--rotate-compilation-buffer)))
    ;; compilation-start sets compilation--start-time inside itself, but the
    ;; variable can be nil when compilation-handle-exit fires (race: process
    ;; exits before Emacs finishes setting up the comint buffer).  Force-set it
    ;; here while still in the synchronous call stack (before the event loop
    ;; can fire the sentinel).
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq compilation--start-time start-time)))
    buf))

(defun my-python/python-execute-file ()
  "Execute a python script in a shell."
  (interactive)
  (let ((cmd (format "%s %s"
                     (executable-find python-shell-interpreter)
                     (shell-quote-argument (file-name-nondirectory buffer-file-name)))))
    (setq my-python--last-cmd cmd
          my-python--last-dir default-directory)
    (my-python--run-cmd cmd)))

(defun my-python/rerun-last-file ()
  "Re-run the last python script executed via `my-python/python-execute-file'."
  (interactive)
  (if my-python--last-cmd
      (let ((default-directory my-python--last-dir))
        (my-python--run-cmd my-python--last-cmd))
    (user-error "No python file has been executed yet")))
