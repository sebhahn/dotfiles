;;; packages.el --- my-mu4e layer packages file for Spacemacs.  -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
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

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `my-mu4e-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-mu4e/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-mu4e/pre-init-PACKAGE' and/or
;;   `my-mu4e/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-mu4e-packages
  '((mu4e :location site)
    mu4e-alert
    (mml2015 :location local)
    org
    persp-mode
    window-purpose)
  )


(defun my-mu4e/init-mml2015 ()
  (use-package mml2015
    :defer t
    :config
    (setq mml2015-signers '("0E8EDF3B")
          mml2015-encrypt-to-self t)))

(defun my-mu4e/pre-init-org ()
  (if mu4e-org-link-support
      (with-eval-after-load 'org
        ;; This is a dirty hack due to mu(4e) 1.8.2 renaming mu4e-meta to
        ;; mu4e-config.  See also
        ;; https://github.com/djcb/mu/commit/cf0f72e4a48ac7029d7f6758b182d4bb559f8f49
        ;; and https://github.com/syl20bnr/spacemacs/issues/15618.  This code
        ;; used to simply read: (require 'mu4e-meta).  We now attempt to load
        ;; mu4e-config.  If this fails, load mu4e-meta.
        (unless (ignore-errors (require 'mu4e-config))
          (ignore-errors (require 'mu4e-meta)))
        (if (version<= mu4e-mu-version "1.3.5")
            (require 'org-mu4e)
          (require 'mu4e-org))
        ;; We require mu4e due to an existing bug https://github.com/djcb/mu/issues/1829
        ;; Note that this bug prevents lazy-loading.
        (if (version<= mu4e-mu-version "1.4.15")
            (require 'mu4e))))
  (if mu4e-org-compose-support
      (progn
        (spacemacs/set-leader-keys-for-major-mode 'mu4e-compose-mode
          "o" 'org-mu4e-compose-org-mode)
        (autoload 'org-mu4e-compose-org-mode "org-mu4e")
        )))

(defun my-mu4e/post-init-window-purpose ()
  (let ((modes))
    (dolist (mode mu4e-list-modes)
      (push (cons mode 'mail) modes))
    (dolist (mode mu4e-view-modes)
      (push (cons mode 'mail-view) modes))
    (purpose-set-extension-configuration
     :mu4e-layer
     (purpose-conf :mode-purposes modes))))

(defun my-mu4e/post-init-persp-mode ()
  (spacemacs|define-custom-layout mu4e-spacemacs-layout-name
    :binding mu4e-spacemacs-layout-binding
    :body
    (progn
      (defun spacemacs-layouts/add-mu4e-buffer-to-persp ()
        (persp-add-buffer (current-buffer)
                          (persp-get-by-name
                           mu4e-spacemacs-layout-name)))
      (spacemacs/add-to-hooks 'spacemacs-layouts/add-mu4e-buffer-to-persp
                              '(mu4e-main-rendered-hook
                                mu4e-headers-mode-hook
                                mu4e-view-rendered-hook
                                mu4e-compose-mode-hook))
      (call-interactively 'mu4e)

      (define-advice mu4e--stop (:after nil kill-mu4e-layout-after-mu4e--stop)
        (when mu4e-spacemacs-kill-layout-on-exit
          (persp-kill mu4e-spacemacs-layout-name))))))

(defun my-mu4e/init-mu4e-alert ()
  (use-package mu4e-alert
    :defer t
    :init (with-eval-after-load 'mu4e
            (when mu4e-enable-notifications
              (mu4e-alert-enable-notifications))
            (when mu4e-enable-mode-line
              (mu4e-alert-enable-mode-line-display)))))

(defun my-mu4e/init-mu4e ()
  "Initialize my package"
  (use-package mu4e
    :commands (mu4e mu4e-compose-new)
    :init
    (progn
      (spacemacs/set-leader-keys "om" 'mu4e)
      (global-set-key (kbd "C-x m") 'mu4e-compose-new))
    :config
    (setq mu4e-change-filenames-when-moving t)

    ;; Send via msmtp: an external SMTP client that does TLS itself.  It validates
    ;; the server cert against the system CA bundle (not a pinned fingerprint), so
    ;; a cert renewal from the same CA is accepted automatically -- no per-send
    ;; warm-up or manual cert-acceptance prompt.
    ;; The account comes from `account default : tu' in ~/.msmtprc, and the
    ;; password from ~/.authinfo.gpg via the `passwordeval' helper.  Sending is
    ;; asynchronous: the message body is handed to a child Emacs which runs msmtp,
    ;; so the UI does not block.  A child error (e.g. msmtp failing) is re-signaled
    ;; in this Emacs by the async package, so it shows up as a message.
    (defun my/mu4e-send-using-msmtp ()
      "Send the current message via msmtp asynchronously."
      (interactive)
      (let ((to          (message-field-value "To"))
            (buf-content (buffer-substring-no-properties
                          (point-min) (point-max))))
        (message "Sending via msmtp to %s..." to)
        (async-start
         `(lambda ()
            (with-temp-buffer
              (insert ,buf-content)
              (set-buffer-multibyte nil)
              (let ((errbuf (generate-new-buffer " *msmtp-errors*")))
                (unwind-protect
                    (let ((exit (call-process-region
                                 (point-min) (point-max)
                                 "/usr/bin/msmtp"
                                 nil errbuf nil
                                 "-t")))
                      (unless (eq exit 0)
                        (error "msmtp failed: %s"
                               (string-trim (with-current-buffer errbuf
                                              (buffer-string))))))
                  (kill-buffer errbuf)))))
         `(lambda (&optional _ignore)
            (message "Sending via msmtp to %s...done" ,to)))))
    (setq send-mail-function 'my/mu4e-send-using-msmtp
          message-send-mail-function 'my/mu4e-send-using-msmtp)

    ;; setup main account
    (setq mu4e-sent-folder "/TU/Sent Items"
          mu4e-drafts-folder "/TU/Drafts"
          user-mail-address "sebastian.hahn@geo.tuwien.ac.at")
    ;; setup information for account switching

    (defvar my-mu4e-account-alist
      '(("TU"
         (mu4e-sent-folder "/TU/Sent Items")
         (mu4e-sent-messages-behavior sent)
         (mu4e-drafts-folder "/TU/Drafts")
         (user-mail-address "sebastian.hahn@geo.tuwien.ac.at")
         (user-full-name  "Sebastian Hahn"))
        ))
    (defun my-mu4e-set-account ()
      "Set the account for composing a message.
When replying to a message whose maildir does not identify an account
(e.g. a refiled/trashed message in `/archive' or `/trash'), fall back to
the default account instead of erroring out and blocking the reply."
      (let* ((account
              (if mu4e-compose-parent-message
                  (or (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                        (when (and (stringp maildir)
                                   (string-match "/\\(.*?\\)/" maildir))
                          (match-string 1 maildir)))
                      (caar my-mu4e-account-alist))
                (completing-read (format "Compose with account: (%s) "
                                         (mapconcat #'(lambda (var) (car var))
                                                    my-mu4e-account-alist "/"))
                                 (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                                 nil t nil nil (caar my-mu4e-account-alist))))
             (account-vars (cdr (assoc account my-mu4e-account-alist))))
        (if account-vars
            (mapc #'(lambda (var)
                      (set (car var) (cadr var)))
                  account-vars)
          (error "No email account found"))))
    (add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)
    (setq message-signature-file "~/.signature"
          message-signature t)

    ;; save message to Sent Messages
    (setq mu4e-sent-messages-behavior 'sent)
    (setq mu4e-refile-folder
          (lambda (msg)
            (cond
             ((string-match "TU" (mu4e-message-field msg :maildir)) "/TU/Archive")
             ;; ((string-match "TU-Git" (mu4e-message-field msg :maildir)) "/TU-Git/INBOX/.erledigt")
             ((mu4e-message-contact-field-matches msg :to "sebastian.hahn@geo.tuwien.ac.at") "/TU/Archive")
             (t  "/archive"))))

    (setq mu4e-trash-folder
          (lambda (msg)
            (cond
             ((string-match "TU" (mu4e-message-field msg :maildir)) "/TU/Deleted Items")
             ;; ((string-match "TU-Git" (mu4e-message-field msg :maildir)) "/TU-Git/Gel&APY-schte Elemente")
             (t  "/trash"))))

    ;; setup some handy shortcuts
    ;; you can quickly switch to your Inbox -- press ``ji''
    ;; then, when you want archive some messages, move them to
    ;; the 'All Mail' folder by pressing ``ma''.

    (setq mu4e-maildir-shortcuts
          '((:maildir "/TU/INBOX" :key ?x)
            (:maildir "/TU/Archive" :key ?a)
            (:maildir "/TU/Drafts" :key ?d)
            (:maildir "/TU/Sent Items" :key ?s)))

    ;; don't keep message buffers around
    (setq message-kill-buffer-on-exit t)
    (setq mu4e-use-fancy-chars t
          mu4e-headers-draft-mark     '("D" . " ")  ; draft
          mu4e-headers-new-mark       '("N" . " ")  ; new
          mu4e-headers-unread-mark    '("U" . " ")  ; unread
          mu4e-headers-unseen-mark    '("u" . " ")  ; unseen
          mu4e-headers-personal-mark  '("p" . " ")   ; personal
          mu4e-headers-list-mark      '("l" . " ")  ; list
          mu4e-headers-seen-mark      '("S" . "")  ; seen
          mu4e-headers-attach-mark    '("A" . " ")  ; attach
          mu4e-headers-flagged-mark   '("F" . " ")  ; flagged
          mu4e-headers-replied-mark   '("R" . " ")  ; replied
          mu4e-headers-passed-mark    '("P" . " ")  ; passed
          mu4e-headers-encrypted-mark '("x" . " ")  ; encrypted
          mu4e-headers-trashed-mark   '("T" . " ")  ; trash
          mu4e-headers-signed-mark    '("s" . " ")) ; signed

        ;;; message view action
    (defun mu4e-msgv-action-view-in-browser (msg)
      "View the body of the message in a web browser."
      (interactive)
      (let ((html (mu4e-msg-field (mu4e-message-at-point t) :body-html))
            (tmpfile (make-temp-file "mu4e-" nil ".html")))
        (unless html
          (delete-file tmpfile)
          (error "No html part for this message"))
        (with-temp-file tmpfile
          (insert
           "<html>"
           "<head><meta http-equiv=\"content-type\""
           "content=\"text/html;charset=UTF-8\">"
           html))
        (browse-url (concat "file://" tmpfile))
        ;; Remove the temp file once the browser has had a chance to load it.
        (run-at-time 5 nil
                     (lambda (f) (ignore-errors (delete-file f)))
                     tmpfile)))

    (add-to-list 'mu4e-view-actions
                 '("View in browser" . mu4e-msgv-action-view-in-browser) t)

    (setq
     mu4e-get-mail-command (concat "mbsync " (my-mu4e/get-sync-channels
                                              (dotfiles/machine-location))))

    ;; update every 10 minutes
    (setq mu4e-update-interval (* 10 60))
    (setq mu4e-index-update-in-background t)

    (setq mu4e-attachment-dir "~/Downloads")

    ;; enable snippets in messages
    (add-hook 'mu4e-compose-mode-hook 'spacemacs/load-yasnippet)
    (add-hook 'mu4e-compose-mode-hook
              (lambda ()
                (local-set-key (kbd "<C-tab>") 'yas-expand)
                (setq-local show-trailing-whitespace nil)))
    (add-hook 'mu4e-view-rendered-hook 'visual-line-mode)

    ;; send mail from address that received it
    (defun my-set-from-address ()
      "Set the From address based on the To address of the original.
Leaves `user-mail-address' untouched when the parent message was not
addressed to one of the known addresses, instead of clobbering it with
nil (which would break sending)."
      (let ((msg mu4e-compose-parent-message)) ;; msg is shorter...
        (when msg
          (let ((addr
                 (cond
                  ((mu4e-message-contact-field-matches msg :to "sebastian.hahn@geo.tuwien.ac.at")
                   "sebastian.hahn@geo.tuwien.ac.at")
                  ((mu4e-message-contact-field-matches msg :to "sebastian.hahn@tuwien.ac.at")
                   "sebastian.hahn@geo.tuwien.ac.at")
                  ((mu4e-message-contact-field-matches msg :to "shahn@ipf.tuwien.ac.at")
                   "sebastian.hahn@geo.tuwien.ac.at"))))
            (when addr
              (setq user-mail-address addr))))))
    ;; Run AFTER `my-mu4e-set-account' (appended), so the account variables are
    ;; set first and the From-address selection can override `user-mail-address'.
    (add-hook 'mu4e-compose-pre-hook #'my-set-from-address t)

    ;; NB: `:body-txt' is not populated for headers-view messages, so this column
    ;; only has data for messages whose body is already loaded; guard nil/short.
    (add-to-list 'mu4e-header-info-custom
                 '(:abstract .
                             ( :name "first n characters of the message"  ;; long name, as seen in the message-view
                               :shortname "Message"           ;; short name, as seen in the headers view
                               :help "First characters of the message body" ;; tooltip
                               :function
                               (lambda (msg)
                                 (let ((body (or (mu4e-message-field msg :body-txt) "")))
                                   (substring body 0 (min 5 (length body))))))))

    ;; set fields to show in headers view
    (setq mu4e-headers-fields '((:human-date . 12)
                                (:flags . 6)
                                (:from-or-to . 22)
                                (:subject)))

    (setq mu4e-headers-date-format "%Y-%m-%d")

    (setq message-citation-line-format "\n\nOn %a, %d %b %Y at %R, %f wrote:\n"
          message-citation-line-function 'message-insert-formatted-citation-line)

    (setq mu4e-hide-index-messages t)

    ;; add new bookmarks

    (add-to-list 'mu4e-bookmarks '(:name "Flagged messages" :query "flag:flagged" :key ?f))
    (add-to-list 'mu4e-bookmarks '(:name "Big messages" :query "size:5M..500M" :key ?b))
    (add-to-list 'mu4e-bookmarks '(:name "Today's send messages" :query "date:today..now AND from:sebastian.hahn@geo.tuwien.ac.at" :key ?s))

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

    ;; `mu4e~headers-defun-mark-for' is a private mu4e macro; define the two
    ;; commands directly via the public `mu4e-headers-mark-and-next' instead of
    ;; depending on mu4e internals.
    (defun mu4e-headers-mark-for-gtag ()
      (interactive)
      (mu4e-headers-mark-and-next 'gtag))
    (defun mu4e-headers-mark-for-rtag ()
      (interactive)
      (mu4e-headers-mark-and-next 'rtag))
    (define-key mu4e-headers-mode-map (kbd "g") 'mu4e-headers-mark-for-gtag)
    (define-key mu4e-headers-mode-map (kbd "e") 'mu4e-headers-mark-for-rtag)

    (require 'org)
    (require 'org-contacts)

    (setq mu4e-org-contacts-file "~/ownCloud/org/roam/areas/agenda/contacts.org")
    (setq org-contacts-files (list mu4e-org-contacts-file))
    (add-to-list 'mu4e-headers-actions
                 '("org-contact-add" . mu4e-action-add-org-contact) t)
    (add-to-list 'mu4e-view-actions
                 '("org-contact-add" . mu4e-action-add-org-contact) t)

    ;; mu4e and org-contacts both install an *exclusive* capf, so whichever
    ;; comes first wins outright and the other is never consulted.  Merge them
    ;; into one table instead: org-contacts entries first, then mu4e's.
    ;;
    ;; org-contacts' own table is a *function* that pre-filters on the string it
    ;; was handed and then returns that list as-is, ignoring
    ;; `completion-regexp-list'.  Styles that narrow through that variable --
    ;; orderless, which compleseus uses -- therefore never filter it.  Hand over
    ;; a plain list instead, which the completion machinery filters itself.
    (defun my/mu4e--display-name (addr)
      "Extract the display name from ADDR, a \"Name <email>\" or \"email\" string."
      (let* ((email-pos (string-match "<[^>]*>\\'" addr))
             (name (if email-pos (string-trim (substring addr 0 email-pos)) addr)))
        (if (and (> (length name) 1)
                 (eq ?\" (aref name 0))
                 (eq ?\" (aref name (1- (length name)))))
            (substring name 1 (1- (length name)))
          name)))

    (defun my/org-contacts-addresses ()
      "Return all `org-contacts' addresses as plain \"Name <email>\" strings.

Excludes group contacts (names starting with `g:'), since those are
meant to be inserted by `my/mu4e-insert-org-contacts-group' rather than
addressed individually (a group such as `g:scat' lists several emails, so
it would otherwise pollute completion)."
      (let ((completion-regexp-list nil))
        (seq-remove
         (lambda (a) (string-prefix-p "g:" (my/mu4e--display-name a)))
         (mapcar #'substring-no-properties
                 ;; the empty prefix matches everything; reuses org-contacts'
                 ;; ignore-property/link handling instead of re-deriving it.
                 (all-completions "" (org-contacts-complete-name ""))))))

    (defun my/mu4e-compose-complete-contact-field ()
      "Complete addresses from `org-contacts' and mu4e's contacts.
Uses the public `mu4e-complete-contact' for mu4e's completions (not mu4e
internals), restricted to contact header fields via the public
`mail-abbrev-in-expansion-header-p'."
      (let ((mail-abbrev-mode-regexp
             "^\\(To\\|B?Cc\\|Reply-To\\|From\\|Sender\\):")
            (mail-header-separator mu4e--header-separator))
        (when (and (mail-abbrev-in-expansion-header-p)
                   (fboundp 'mu4e-complete-contact))
          (when-let* ((res (mu4e-complete-contact)))
            (let* ((start (nth 0 res))
                   (end (nth 1 res))
                   (prefix (buffer-substring-no-properties start (point)))
                   (org (my/org-contacts-addresses))
                   (org-names (mapcar #'downcase (mapcar #'my/mu4e--display-name org)))
                   (extra (seq-remove
                           (lambda (c)
                             (member (downcase (my/mu4e--display-name c)) org-names))
                           (all-completions prefix (nth 2 res)))))
              (list start end (append org extra)))))))

    ;; Run LAST on `mu4e-compose-mode-hook' (depth 100).  org-contacts installs
    ;; `org-contacts-message-complete-function' on both `message-mode-hook' and
    ;; its own `mu4e-compose-mode-hook' entry, and mu4e adds its own exclusive
    ;; completion in the mode body.  Running after all of them lets us strip both
    ;; and install the merged function; otherwise org-contacts' exclusive function
    ;; wins and the merge is never consulted.  If the public `mu4e-complete-contact'
    ;; is unavailable, keep org-contacts' own capf so address completion still
    ;; works (the merged one needs that public API).
    (add-hook 'mu4e-compose-mode-hook
              (lambda ()
                (setq-local completion-styles
                            (remove 'substring completion-styles))
                (remove-hook 'completion-at-point-functions
                             #'mu4e--compose-complete-contact-field t)
                (if (fboundp 'mu4e-complete-contact)
                    (progn
                      (remove-hook 'completion-at-point-functions
                                   #'org-contacts-message-complete-function t)
                      (add-hook 'completion-at-point-functions
                                #'my/mu4e-compose-complete-contact-field -10 t))
                  (remove-hook 'completion-at-point-functions
                               #'my/mu4e-compose-complete-contact-field t)))
              t 100)

    ;; from http://www.djcbsoftware.nl/code/mu/mu4e/Attaching-files-with-dired.html
    (require 'gnus-dired)
    ;; gnus-dired's built-in `gnus-dired-mail-buffers' only special-cases
    ;; `message-user-agent' and `gnus-user-agent'; for `mu4e-user-agent' it falls
    ;; back to plain `mail-mode' buffers and would miss mu4e-compose buffers
    ;; (which derive from message-mode).  Advise it instead of redefining it, so
    ;; gnus-dired's own logic still applies to the other modes.
    (with-eval-after-load 'gnus-dired
      (advice-add 'gnus-dired-mail-buffers :around
                  (lambda (orig)
                    (if (eq gnus-dired-mail-mode 'mu4e-user-agent)
                        (progn (require 'message) (message-buffers))
                      (funcall orig)))))

    (setq gnus-dired-mail-mode 'mu4e-user-agent)
    (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

    ;; set mu4e as default
    (setq mail-user-agent 'mu4e-user-agent)

    ;; spacemacs stuff
    (evilified-state-evilify-map mu4e-main-mode-map
      :mode mu4e-main-mode
      :bindings
      (kbd "j") 'mu4e-search-maildir
      (kbd "C-j") 'next-line
      (kbd "C-k") 'previous-line)

    (evilified-state-evilify-map mu4e-headers-mode-map
      :mode mu4e-headers-mode
      :bindings
      (kbd "C-c w") 'my-mu4e/refresh-work-only
      (kbd "C-j") 'mu4e-headers-next
      (kbd "C-k") 'mu4e-headers-prev
      (kbd "J") (lambda ()
                  (interactive)
                  (mu4e-headers-mark-thread nil '(read))))

    (evilified-state-evilify-map mu4e-view-mode-map
      :mode mu4e-view-mode
      :bindings
      (kbd "J") (lambda ()
                  (interactive)
                  (mu4e-headers-mark-thread nil '(read)))
      (kbd "gu") 'mu4e-view-go-to-url
      (kbd "RET") 'my/mu4e-view-ret
      (kbd "x") 'mu4e-mark-execute-all
      (kbd "C-j") 'mu4e-headers-next
      (kbd "C-k") 'mu4e-headers-prev)

    (defun my/mu4e-insert-org-contacts-group ()
      "Prompt for an org-contacts group (g:-prefixed heading) and insert its addresses."
      (interactive)
      (let* ((all (org-contacts-db))
             (groups (cl-remove-if-not
                      (lambda (c) (string-prefix-p "g:" (car c)))
                      all))
             (name (completing-read "Group: " (mapcar #'car groups)))
             (entry (assoc name groups))
             ;; org-contacts-db entries are (name email props); the email property
             ;; list is the 3rd element.  Guarded against entry/layout changes so
             ;; a missing group doesn't error.
             (props (and entry (nth 2 entry)))
             (emails (and props
                           (cdr (assoc-string org-contacts-email-property props)))))
        (when emails (insert emails))))

    (spacemacs/set-leader-keys-for-major-mode 'mu4e-compose-mode
      "t" 'message-goto-to
      "m" 'message-goto-body
      "b" 'message-goto-bcc
      "c" 'message-goto-cc
      "u" 'message-goto-subject
      "d" 'message-kill-buffer
      "w" 'message-dont-send         ; saves as draft
      "x" 'message-send-and-exit
      "f" 'mml-attach-file
      "e" 'mml-secure-message-encrypt-pgpmime
      "s" 'mml-secure-message-sign-pgpmime
      "g" 'my/mu4e-insert-org-contacts-group)

    (with-eval-after-load "mm-decode"
      (add-to-list 'mm-discouraged-alternatives "text/html")
      (add-to-list 'mm-discouraged-alternatives "text/richtext"))

    (setq shr-color-visible-luminance-min 80)

    (setq gnus-unbuttonized-mime-types nil)

    (setq mu4e-search-include-related nil)

    (setq mu4e-read-option-use-builtin nil
          mu4e-completing-read-function 'completing-read
          mu4e-compose-complete-only-personal t
          mu4e-compose-complete-only-after "2024-01-01")
    )
  )
