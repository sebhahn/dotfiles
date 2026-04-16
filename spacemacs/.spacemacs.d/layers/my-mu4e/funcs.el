;; (defun my-mu4e/get-sync-channels (location)
;;   (let ((sync-channels '((home . "tu tu-git")
;;                          (work . "tu tu-git"))))
;;     (cdr (assoc location sync-channels))))

;; (defun my-mu4e/refresh-work-only ()
;;   (interactive)
;;   (let ((mu4e-get-mail-command "mbsync tu tu-git"))
;;     (mu4e-update-mail-and-index nil)))

(defun my-mu4e/get-sync-channels (location)
  (let ((sync-channels '((home . "tu")
                         (work . "tu"))))
    (cdr (assoc location sync-channels))))

(defun my-mu4e/refresh-work-only ()
  (interactive)
  (let ((mu4e-get-mail-command "mbsync tu"))
    (mu4e-update-mail-and-index nil)))
