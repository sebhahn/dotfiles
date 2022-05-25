
(defun my-python/python-execute-file (arg)
  "Execute a python script in a shell."
  (interactive "P")
  ;; set compile command to buffer-file-name
  ;; universal argument put compile buffer in comint mode
  (let ((universal-argument t)
        (compile-command (format "%s %s"
                                 (spacemacs/pyenv-executable-find python-shell-interpreter)
                                 (shell-quote-argument (file-name-nondirectory buffer-file-name)))))

    (if arg
      (call-interactively 'compile)
      (compile compile-command t)
      (print compile-command)
      (with-current-buffer (get-buffer "*compilation*")
        (inferior-python-mode)))
    ))

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

;; (with-eval-after-load 'compile
;;   (add-hook 'compilation-mode-hook
;;     (lambda ()
;;      (rename-uniquely)
;;      (setq buffer-read-only nil))))

;; (setq compilation-buffer-name-function
;;       (lambda (name-of-mode)
;;         (generate-new-buffer-name
;;          (concat "*" (downcase name-of-mode) "*"))))
