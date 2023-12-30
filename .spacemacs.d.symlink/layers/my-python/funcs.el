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

(defun my-python/python-execute-file ()
  "Execute a python script in a shell."
  (interactive)
  (let* ((universal-argument t)
         (compile-command (format "%s %s"
                                  (spacemacs/pyenv-executable-find python-shell-interpreter)
                                  (shell-quote-argument (file-name-nondirectory buffer-file-name))))
         (compilation-buffer-name "*compilation*"))

    ;; Ensure a new compilation buffer is created
    (with-current-buffer (get-buffer-create compilation-buffer-name)
      (rename-buffer (generate-new-buffer-name compilation-buffer-name)))

    (compile compile-command)

    ;; Switch to the new compilation buffer
    (with-current-buffer compilation-buffer-name
      (inferior-python-mode))))

;; (with-eval-after-load 'compile
;;   (add-hook 'compilation-mode-hook
;;     (lambda ()
;;      (rename-uniquely)
;;      (setq buffer-read-only nil))))

;; (setq compilation-buffer-name-function
;;       (lambda (name-of-mode)
;;         (generate-new-buffer-name
;;          (concat "*" (downcase name-of-mode) "*"))))
