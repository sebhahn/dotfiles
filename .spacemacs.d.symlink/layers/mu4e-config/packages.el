;;; packages.el --- mu4e-config Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(defconst mu4e-config-packages
  '(
    ;; package names go here
    helm-mu
    mu4e-maildirs-extension
    (mu4e :location local)
    (org-mu4e :location local)
    (mml2015 :location local)
    persp-mode
    ))

;; For each package, define a function mu4e-config/init-<package-name>
;;
(defun mu4e-config/init-helm-mu ()
  (use-package helm-mu
    :defer t
    :commands helm-mu
    :init (spacemacs/set-leader-keys "oM" 'helm-mu
                                     "oC" 'helm-mu-contacts))
  )


(defun mu4e-config/init-mu4e-maildirs-extension ()
  (use-package mu4e-maildirs-extension
    :defer t))


(defun mu4e-config/init-mml2015 ()
  (use-package mml2015
    :defer t
    :config
    (setq mml2015-signers '("0E8EDF3B")
          mml2015-encrypt-to-self t)))

(defvar work-sig "Dipl.-Ing. Sebastian Hahn
Research Group Microwave Remote Sensing (MRS)
Department of Geodesy and Geoinformation (GEO)
Technische Universität Wien (TU Wien)
Wiedner Hauptstr. 8-10/E120, 1040 Vienna, Austria
Phone: +43-1-58801-12240
http://mrs.geo.tuwien.ac.at/")

;; For each package, define a function mu4e-config/init-<package-name>
;;
(defun mu4e-config/init-mu4e ()
   "Initialize my package"
   (use-package mu4e
     :defer t
     :commands mu4e
     :init
     (spacemacs/set-leader-keys "om" 'mu4e)
     :config
        (setq mu4e-view-show-images t)
        ;; use imagemagick if available
        (when (fboundp 'imagemagick-register-types)
        (imagemagick-register-types))
        (setq mu4e-html2text-command "lynx -dump -width 100 -stdin --display_charset=utf-8")
        (setq mu4e-change-filenames-when-moving t)
        ;; default
        ;; sending mail -- replace USERNAME with your gmail username
        ;; also, make sure the gnutls command line utils are installed
        ;; package 'gnutls-bin' in Debian/Ubuntu

        (require 'smtpmail-async)
        (setq send-mail-function 'async-smtpmail-send-it
              message-send-mail-function 'async-smtpmail-send-it)

        ;; (setq send-mail-function 'smtpmail-send-it
        ;;       message-send-mail-function 'smtpmail-send-it)

        (setq mu4e-maildir "~/mbsync")
        ;; setup main account
        (setq mu4e-sent-folder "/TU/Sent Items"
              mu4e-drafts-folder "/TU/Drafts"
              user-mail-address "sebastian.hahn@geo.tuwien.ac.at"
              smtpmail-default-smtp-server "mail.intern.tuwien.ac.at"
              smtpmail-smtp-server "mail.intern.tuwien.ac.at"
              smtpmail-auth-credentials '(("mail.intern.tuwien.ac.at" 587 "shahn" nil))
              smtpmail-smtp-service 587)
        ;; setup information for account switching
        (defvar mu4e-config-account-alist
          '(("TU"
             (mu4e-sent-folder "/TU/Sent Items")
             (mu4e-sent-messages-behavior sent)
             (mu4e-drafts-folder "/TU/Drafts")
             (user-mail-address "sebastian.hahn@geo.tuwien.ac.at")
             (user-full-name  "Sebastian Hahn")
             (smtpmail-smtp-user "shahn")
             (smtpmail-smtp-server "mail.intern.tuwien.ac.at")
             (smtpmail-auth-credentials '(("mail.intern.tuwien.ac.at" 587 "shahn" nil)))
             (smtpmail-smtp-service 587))
            ("TU-Git"
             (mu4e-sent-folder "/TU-Git/Sent Items")
             (mu4e-sent-messages-behavior sent)
             (mu4e-drafts-folder "/TU-Git/Drafts")
             (user-mail-address "git@geo.tuwien.ac.at")
             (user-full-name  "Sebastian Hahn")
             (smtpmail-smtp-user "shahn")
             (smtpmail-smtp-server "mail.intern.tuwien.ac.at")
             (smtpmail-auth-credentials '(("mail.intern.tuwien.ac.at" 587 "shahn" nil)))
             (smtpmail-smtp-service 587))
            ))
        (defun mu4e-config-set-account ()
          "Set the account for composing a message."
          (let* ((account
                  (if mu4e-compose-parent-message
                      (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                        (string-match "/\\(.*?\\)/" maildir)
                        (match-string 1 maildir))
                    (completing-read (format "Compose with account: (%s) "
                                             (mapconcat #'(lambda (var) (car var))
                                                        mu4e-config-account-alist "/"))
                                     (mapcar #'(lambda (var) (car var)) mu4e-config-account-alist)
                                     nil t nil nil (caar mu4e-config-account-alist))))
                 (account-vars (cdr (assoc account mu4e-config-account-alist))))
            (if account-vars
                (mapc #'(lambda (var)
                          (set (car var) (cadr var)))
                      account-vars)
              (error "No email account found"))))
        (add-hook 'mu4e-compose-pre-hook 'mu4e-config-set-account)
        ;; set signature based on account

        (defun my-set-signature ()
          "My settings for signatures."
          (setq message-signature
                (cond
                 ((string-match "sebastian.hahn" user-mail-address) work-sig)
                 (t ""))))


        ;; handle signatures with message buffer
        (setq mu4e-compose-signature-auto-include nil)
        (add-hook 'mu4e-compose-mode-hook
                  (defun my-do-compose-stuff ()
                    "My settings for message composition."
                    (my-set-signature)
                    (message-insert-signature)
                    (message-goto-to)
                    ;; enable signing of emails by default
                    ;; (mml-secure-message-sign-pgpmime)
                    ))

        ;; save message to Sent Messages
        (setq mu4e-sent-messages-behavior 'sent)
        (setq mu4e-refile-folder
              (lambda (msg)
                (cond
                 ((string-match "TU" (mu4e-message-field msg :maildir)) "/TU/Archives")
                 ((string-match "TU-Git" (mu4e-message-field msg :maildir)) "/TU-Git/INBOX/.erledigt")
                 ;; ((string-match "Personal" (mu4e-message-field msg :maildir)) "/Personal/[Gmail].All Mail")
                  ;; messages to the mu mailing list go to the /mu folder
                  ;; ((mu4e-message-contact-field-matches msg :to "cpaulik@gmail.com") "/Personal/Archive")
                 ;; messages sent directly to me go to /archive
                 ;; also `mu4e-user-mail-address-p' can be used
                 ((mu4e-message-contact-field-matches msg :to "sebastian.hahn@geo.tuwien.ac.at") "/TU/Archives")
                 ;; everything else goes to /archive
                 ;; important to have a catch-all at the end!
                 (t  "/archive"))))

        (setq mu4e-trash-folder
              (lambda (msg)
                (cond
                 ((string-match "TU" (mu4e-message-field msg :maildir)) "/TU/Deleted Items")
                 ((string-match "TU-Git" (mu4e-message-field msg :maildir)) "/TU-Git/Gel&APY-schte Elemente")
                 ;; ((string-match "Personal" (mu4e-message-field msg :maildir)) "/Personal/[Gmail].Trash")
                  ;; messages to the mu mailing list go to the /mu folder
                 ;; everything else goes to /archive
                 ;; important to have a catch-all at the end!
                 (t  "/trash"))))

        ;; setup some handy shortcuts
        ;; you can quickly switch to your Inbox -- press ``ji''
        ;; then, when you want archive some messages, move them to
        ;; the 'All Mail' folder by pressing ``ma''.

        (setq mu4e-maildir-shortcuts
            '((:maildir "/TU/INBOX" :key ?x)
              (:maildir "/TU/Archives" :key ?a)
              (:maildir "/TU/it" :key ?i)
              (:maildir "/TU/geo" :key ?g)
              (:maildir "/TU/hsaf" :key ?h)))

        ;; something about ourselves
        (setq mu4e-compose-signature ""
            mu4e-user-mail-address-list
            '("sebastian.hahn@geo.tuwien.ac.at"))

        ;; don't keep message buffers around
        (setq message-kill-buffer-on-exit t)
        (setq mu4e-use-fancy-chars t
                mu4e-headers-draft-mark     '("D" . " ")  ; draft
                mu4e-headers-new-mark       '("N" . "")  ; new
                mu4e-headers-unread-mark    '("U" . " ")  ; unread
                mu4e-headers-unseen-mark    '("u" . " ")  ; unseen
                ;; mu4e-headers-seen-mark      '("S" . " ")  ; seen
                mu4e-headers-seen-mark      '("S" . "")  ; seen
                mu4e-headers-attach-mark    '("A" . " ")  ; attach
                mu4e-headers-flagged-mark   '("F" . " ")  ; flagged
                mu4e-headers-replied-mark   '("R" . " ")  ; replied
                mu4e-headers-passed-mark    '("P" . " ")  ; passed
                mu4e-headers-encrypted-mark '("x" . " ")  ; encrypted
                mu4e-headers-trashed-mark   '("T" . " ")  ; trash
                mu4e-headers-signed-mark    '("s" . " ")) ; signed

        ;;; message view action
        (defun mu4e-msgv-action-view-in-browser (msg)
            "View the body of the message in a web browser."
            (interactive)
            (let ((html (mu4e-msg-field (mu4e-message-at-point t) :body-html))
                    (tmpfile (format "%s/%d.html" temporary-file-directory (random))))
                (unless html (error "No html part for this message"))
                (with-temp-file tmpfile
                (insert
                "<html>"
                "<head><meta http-equiv=\"content-type\""
                "content=\"text/html;charset=UTF-8\">"
                html))
                (browse-url (concat "file://" tmpfile))))

            (add-to-list 'mu4e-view-actions
                        '("View in browser" . mu4e-msgv-action-view-in-browser) t)

        (setq
         mu4e-get-mail-command (concat "mbsync " (mu4e-config/get-sync-channels
                                                 (dotfiles/machine-location))))

        ;; update every 5 minutes
        (setq mu4e-update-interval (* 5 60))
        (setq mu4e-index-update-in-background t)
        (setq mu4e-compose-dont-reply-to-self t)
        ;; (setq org-mu4e-link-query-in-headers-mode t)

        (setq mu4e-attachment-dir "~/Downloads")

        ;; enable snippets in messages
        (add-hook 'mu4e-compose-mode-hook 'spacemacs/load-yasnippet)
        (add-hook 'mu4e-view-mode-hook 'visual-line-mode)

        ;; send mail from address that received it
        (add-hook 'mu4e-compose-pre-hook
                (defun my-set-from-address ()
                    "Set the From address based on the To address of the original."
                    (let ((msg mu4e-compose-parent-message)) ;; msg is shorter...
                    (if msg
                        (setq user-mail-address
                                (cond
                                ((mu4e-message-contact-field-matches msg :to "sebastian.hahn@geo.tuwien.ac.at")
                                "sebastian.hahn@geo.tuwien.ac.at")
                                ((mu4e-message-contact-field-matches msg :to "sebastian.hahn@tuwien.ac.at")
                                 "sebastian.hahn@geo.tuwien.ac.at")
                                ((mu4e-message-contact-field-matches msg :to "shahn@ipf.tuwien.ac.at")
                                "sebastian.hahn@geo.tuwien.ac.at")
                                ))))))

        ;; not possible because message body is not available in headers view
        (add-to-list 'mu4e-header-info-custom
        '(:abstract .
            ( :name "first n characters of the message"  ;; long name, as seen in the message-view
            :shortname "Message"           ;; short name, as seen in the headers view
            :help "Number of recipients for this message" ;; tooltip
            :function
            (lambda (msg)
        (subseq (mu4e-message-field msg :body-txt) 0 5)
            ))))

        ;; set fields to show in headers view
        (setq mu4e-headers-fields '((:human-date . 12)
                                    (:flags . 6)
                                    (:from-or-to . 22)
                                    (:subject)))

        (setq mu4e-headers-date-format "%Y-%m-%d")


        ;; (setq mu4e-compose-format-flowed t)
        (setq mu4e-view-show-addresses 't)

        (setq message-citation-line-format "On %a, %d %b %Y at %R, %f wrote:\n"
              message-citation-line-function 'message-insert-formatted-citation-line)

        (setq mu4e-hide-index-messages t)

        ;; add new bookmarks
        (add-to-list 'mu4e-bookmarks '(:name "Flagged messages" :query "flag:flagged" :key ?f))
        (add-to-list 'mu4e-bookmarks '(:name "Big messages" :query "size:5M..500M" :key ?b))
        (add-to-list 'mu4e-bookmarks '(:name "Today's send messages" :query "date:today..now from:sebastian.hahn@geo.tuwien.ac.at" :key ?s))

        (add-to-list 'mu4e-marks
                     '(gtag
                       :char       "g"
                       :prompt     "gtag"
                       :ask-target (lambda () (read-string "tag: "))
                       :action     (lambda (docid msg target)
                                   (mu4e-action-retag-message msg (concat "+" target)))))

        (add-to-list 'mu4e-marks
                     '(rtag
                       :char       "r"
                       :prompt     "rtag"
                       :ask-target (lambda () (read-string "remove tag: "))
                       :action     (lambda (docid msg target)
                                     (mu4e-action-retag-message msg (concat "-" target)))))

        ;; (add-to-list 'mu4e-marks
        ;;              '(archive
        ;;                :char       "A"
        ;;                :prompt     "Archive"
        ;;                :show-target (lambda (target) "archive")
        ;;                :action      (lambda (docid msg target)
        ;;                               ;; must come before proc-move since retag runs
        ;;                               ;; 'sed' on the file
                                      ;; (mu4e-action-retag-message msg "-\\Inbox")
                                      ;; (mu4e~proc-move docid nil "+S-u-N"))))

        (mu4e~headers-defun-mark-for gtag)
        (mu4e~headers-defun-mark-for rtag)
        ;; (mu4e~headers-defun-mark-for archive)
        (define-key mu4e-headers-mode-map (kbd "g") 'mu4e-headers-mark-for-tag)
        (define-key mu4e-headers-mode-map (kbd "e") 'mu4e-headers-mark-for-tag)
        ;; (define-key mu4e-headers-mode-map (kbd "A") 'mu4e-headers-mark-for-archive)

        ;; (defun org-mu4e-store-link ()
        ;; "Store a link to a mu4e query or message."
        ;; (cond
        ;; ;; storing links to queries
        ;; ((eq major-mode 'mu4e-headers-mode)
        ;;     (let* ((query (mu4e-last-query))
        ;;         desc link)
        ;; (org-store-link-props :type "mu4e" :query query)
        ;; (setq
        ;;     desc (concat "mu4e:query:" query)
        ;;     link desc)
        ;; (org-add-link-props :link link :description desc)
        ;; link))
        ;;     ;; storing links to messages
        ;; ((eq major-mode 'mu4e-view-mode)
        ;;     (let* ((msg  (mu4e-message-at-point))
        ;;     (msgid   (or (plist-get msg :message-id) "<none>"))
        ;;     (from (car (car (mu4e-message-field msg :from))))
        ;;     (to (car (car (mu4e-message-field msg :to))))
        ;;     (subject (mu4e-message-field msg :subject))
        ;;     link)
        ;;     (setq link (concat "mu4e:msgid:" msgid))
        ;;     (org-store-link-props :type "mu4e" :link link
        ;;             :message-id msgid)
        ;;     (setq link (concat "mu4e:msgid:" msgid))
        ;;     (org-store-link-props
        ;;     :type "mu4e" :from from :to to :subject subject
        ;;             :message-id msgid)

        ;;     (org-add-link-props :link link
        ;;             :description (funcall org-mu4e-link-desc-func msg))
        ;;     link))))

        (require 'org)
        (require 'org-mu4e)
        (require 'org-contacts)

        (setq mu4e-org-contacts-file "~/ownCloud/org/contacts.org")
        (add-to-list 'mu4e-headers-actions
                     '("org-contact-add" . mu4e-action-add-org-contact) t)
        (add-to-list 'mu4e-view-actions
                     '("org-contact-add" . mu4e-action-add-org-contact) t)

        ;; use helm for navigation
        (setq  mu4e-completing-read-function 'completing-read)

        ;; setup helm and dired for attachments

        (require 'gnus-dired)
        ;; make the `gnus-dired-mail-buffers' function also work on
        ;; message-mode derived modes, such as mu4e-compose-mode
        (defun gnus-dired-mail-buffers ()
          "Return a list of active message buffers."
          (let (buffers)
            (save-current-buffer
              (dolist (buffer (buffer-list t))
                (set-buffer buffer)
                (when (and (derived-mode-p 'message-mode)
                           (null message-sent-message-via))
                  (push (buffer-name buffer) buffers))))
            (nreverse buffers)))

        (setq gnus-dired-mail-mode 'mu4e-user-agent)

        (require 'helm)
        (add-to-list 'helm-find-files-actions
                     '("Attach files for mu4e" .
                       helm-mu4e-attach) t)

        (defun helm-mu4e-attach (_file)
          (gnus-dired-attach (helm-marked-candidates)))

        ;; store all attachments of an email into the same folder
        (setq mu4e-save-multiple-attachments-without-asking t)

        ;; set mu4e as default
        (setq mail-user-agent 'mu4e-user-agent)

        ;; enable mu4e maildirs extension
        (mu4e-maildirs-extension)

        ;; spacemacs stuff
        (evilified-state-evilify-map mu4e-main-mode-map
          :mode mu4e-main-mode
          :bindings
          (kbd "TAB") 'mu4e-maildirs-extension-toggle-maildir-at-point)

        (evilified-state-evilify-map mu4e-headers-mode-map
          :mode mu4e-headers-mode
          :bindings
          (kbd "C-c w") 'mu4e-config/refresh-work-only)

        (evilified-state-evilify-map mu4e-view-mode-map
          :mode mu4e-view-mode
          :bindings
                 (kbd "RET") 'browse-url-at-point
                 (kbd "x") 'mu4e-mark-execute-all
                 (kbd "C-j") 'mu4e-view-headers-next
                 (kbd "C-k") 'mu4e-view-headers-prev)

        (spacemacs/set-leader-keys-for-major-mode 'mu4e-compose-mode
          "t" 'message-goto-to
          "m" 'message-goto-body
          "b" 'message-goto-bcc
          "c" 'message-goto-cc
          "u" 'message-goto-subject
          "d" 'message-kill-buffer
          "e" 'mml-secure-message-encrypt-pgpmime
          "s" 'mml-secure-message-sign-pgpmime)
     )
   )

(defun mu4e-config/init-org-mu4e ()
  "init org integration for mu4e"
  (use-package org-mu4e
    :defer nil)
  )

(defun mu4e-config/post-init-persp-mode ()
  (spacemacs|define-custom-layout "@Mail"
    :binding "m"
    :body
    (mu4e)))
