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

;; ---------------------------------------------------------------------------
;; Fix replies to messages whose From/To/Cc/Reply-To display name is an
;; UNQUOTED string containing a comma (technically invalid per RFC 5322, e.g.
;; GitHub sending  `From: Mikolka-Flöry, Sebastian <seb@example.com>`).
;;
;; Emacs' message-reply (message.el) treats that comma as a recipient
;; separator and mangles the reply into e.g.  To: "Mikolka-Flöry" and
;; Cc: "Sebastian <...>, me@...".  Quoting the display name fixes it.
;; ---------------------------------------------------------------------------

(defun my-mu4e/fix-comma-name (addr)
  "Quote an unquoted display name that contains a comma in ADDR.
Leaves already-quoted names and comma-free names untouched."
  (let ((trimmed (string-trim addr))
        (addr-beg (string-match "<[^>]*>" addr)))
    (if (and addr-beg
             (> addr-beg 0)
             (not (eq ?\" (aref trimmed 0)))
             (string-match-p "," (substring addr 0 addr-beg)))
        (format "\"%s\" %s"
                (string-trim (substring addr 0 addr-beg))
                (string-trim (substring addr addr-beg)))
      addr)))

(defun my-mu4e/fix-reply-headers-in-place ()
  "Quote, in place, unquoted comma-containing display names in header lines.
Processes the From, Reply-To, To and Cc fields of the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((end (save-excursion (re-search-forward "^$" nil t) (point))))
      (goto-char (point-min))
      (while (and (< (point) end)
                  (re-search-forward "^\\(From\\|Reply-To\\|To\\|Cc\\):[ \t]*" end t))
        (let ((mend (match-end 0))
              (eol (line-end-position)))
          (when (< mend eol)
            (let* ((val (string-trim (buffer-substring-no-properties mend eol)))
                   (new (my-mu4e/fix-comma-name val)))
              (when (not (string-equal new val))
                (delete-region mend eol)
                (insert " " new))))
          (goto-char (line-beginning-position 2)))))))

(defun my-mu4e/fix-message-reply-from (&rest _args)
  "Quote unquoted comma-containing display names before composing a reply."
  (my-mu4e/fix-reply-headers-in-place))

;; mu4e composes replies in a hidden draft buffer holding the decoded parent
;; message, so normalizing its headers pre-reply is safe/clean.
(advice-add 'message-reply :before #'my-mu4e/fix-message-reply-from)
