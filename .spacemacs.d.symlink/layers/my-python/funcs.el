
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

;; (with-eval-after-load 'compile
;;   (add-hook 'compilation-mode-hook
;;     (lambda ()
;;      (rename-uniquely)
;;      (setq buffer-read-only nil))))

;; (setq compilation-buffer-name-function
;;       (lambda (name-of-mode)
;;         (generate-new-buffer-name
;;          (concat "*" (downcase name-of-mode) "*"))))
