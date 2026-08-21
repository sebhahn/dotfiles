;;; -*- lexical-binding: t -*-
;; (defun my-mu4e/get-sync-channels (location)
;;   (let ((sync-channels '((home . "tu tu-git")
;;                          (work . "tu tu-git"))))
;;     (cdr (assoc location sync-channels))))

;; (defun my-mu4e/refresh-work-only ()
;;   (interactive)
;;   (let ((mu4e-get-mail-command "mbsync tu tu-git"))
;;     (mu4e-update-mail-and-index nil)))

(defun my-mu4e/get-sync-channels (location)
  "Return the mbsync channels to sync at LOCATION.
Both locations currently sync the same channels; LOCATION is still honored
so per-machine channels can be reintroduced by editing `sync-channels'.
An unknown LOCATION -- a machine missing from `dotfiles/machine-location'
-- falls back to the home channels instead of returning nil, which would
leave `mu4e-get-mail-command' as a bare \"mbsync\" with no channel."
  (let ((sync-channels '((home . "tu")
                         (work . "tu"))))
    (or (cdr (assoc location sync-channels))
        (cdr (assq 'home sync-channels)))))

(defun my-mu4e/refresh-work-only ()
  (interactive)
  (let ((mu4e-get-mail-command "mbsync tu"))
    (mu4e-update-mail-and-index nil)))
