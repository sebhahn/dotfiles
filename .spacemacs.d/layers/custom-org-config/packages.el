;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar custom-org-config-packages
  '(org-plus-contrib
    (org :location built-in)
    (org-ac :location built-in)
    cdlatex
    ;; package custom-org-configs go here
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar custom-org-config-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function custom-org-config/init-<package-custom-org-config>
;;

(defun custom-org-config/post-init-org-plus-contrib ()
  )

(defun custom-org-config/init-cdlatex()
  )

(defun custom-org-config/init-org-ac()
  (use-package org-ac
    :defer t
    :config
    (progn
      (org-ac/config-default))))


(defun custom-org-config/pre-init-org ()
  ;;   "Initialize my package"
  (use-package org
    :defer t
    :commands (org-mode
               org-edit-src-exit
               org-agenda
               org-capture
               org-toggle-latex-fragment
               org-store-link
               org-iswitchb
               org-clock-goto
               org-clock-in))

  (spacemacs|use-package-add-hook org
    :post-init
    (progn
      (spacemacs/set-leader-keys
        "m'" 'org-edit-src-exit
        "oo" 'org-agenda
        "oc" 'org-capture
        "osr" 'org-refile
        "osa" 'org-toggle-archive-tag)

      (spacemacs/set-leader-keys-for-major-mode 'org-mode "sr" 'org-refile)

      (spacemacs/declare-prefix "ol" "org-link")
      (spacemacs/set-leader-keys
        "ols" 'org-store-link
        "oli" 'org-insert-link)

      (spacemacs/declare-prefix-for-mode 'org-mode "l" "org-link")
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "ls" 'org-store-link
        "li" 'org-insert-link)

      (spacemacs/declare-prefix "oj" "org-journal")
      (spacemacs/set-leader-keys
        "ojk" 'org-journal-new-entry
        "oje" 'org-journal-new-scheduled-entry
        "ojs" 'org-journal-search
        "ojn" 'org-journal-next-entry
        "ojd" 'org-journal-display-entry
        "ojp" 'org-journal-previous-entry
        "ojf" 'org-journal-open-current-journal-file
        "oTh" 'org-toggle-heading)

      (spacemacs/declare-prefix "ok" "org-agenda")
      (spacemacs/set-leader-keys
        "oar" 'org-agenda-clockreport-mode
        "oas" 'org-agenda-sunrise-sunset
        "oat" 'spacemacs/org-agenda-transient-state/body
        "oak" 'org-agenda-show-clocking-issues
        "oan" 'org-agenda-next-line
        "oag" 'org-agenda-toggle-time-grid
        "oafr" 'org-agenda-filter-remove-all
        "oaft" 'org-agenda-filter-by-tag)

      (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
        "r" 'org-agenda-clockreport-mode
        "t" 'spacemacs/org-agenda/transient-state/body
        "k" 'org-agenda-show-clocking-issues)

      (spacemacs/declare-prefix "ok" "org-clock")
      (spacemacs/set-leader-keys
        "oki" 'org-clock-in
        "oko" 'org-clock-out
        "okc" 'org-clock-cancel
        "oke" 'org-set-effort
        "okm" 'org-modify-effort-estimate
        "okg" 'org-clock-goto
        "okj" 'org-clock-jump-to-current-clock
        "okr" 'org-clock-report
        "okR" 'org-resolve-clocks
        "okd" 'org-clock-mark-default-task
        "okk" 'org-clock-display
        "okv" 'org-clock-remove-overlays
        "okl" 'org-clock-in-last)

      (spacemacs/declare-prefix-for-mode 'org-mode "k" "org-clock")
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "is" 'org-insert-subheading-respect-content
        "ki" 'org-clock-in
        "ko" 'org-clock-out
        "kc" 'org-clock-cancel
        "ke" 'org-set-effort
        "km" 'org-modify-effort-estimate
        "kg" 'org-clock-goto
        "kj" 'org-clock-jump-to-current-clock
        "kr" 'org-clock-report
        "kR" 'org-resolve-clocks
        "kd" 'org-clock-mark-default-task
        "kk" 'org-clock-display
        "kv" 'org-clock-remove-overlays
        "kl" 'org-clock-in-last)

      (spacemacs/declare-prefix-for-mode 'org-mode "j" "org-journal")
      (spacemacs/set-leader-keys
        "jk" 'org-journal-new-entry
        "je" 'org-journal-new-scheduled-entry
        "js" 'org-journal-search
        "jn" 'org-journal-next-entry
        "jd" 'org-journal-display-entry
        "jp" 'org-journal-previous-entry
        "jf" 'org-journal-open-current-journal-file)

      (spacemacs/set-leader-keys
        "oS" 'org-download-screenshot
        "oC" 'org-download-clipboard
        "oY" 'org-download-yank)

      (spacemacs/set-leader-keys-for-major-mode 'org-mode "C-l" 'org-toggle-latex-fragment))

    :post-config
    (progn

      (defun org-insert-subheading-respect-content (&optional)
        (interactive)
        (let ((org-insert-heading-respect-content t))
          (org-insert-subheading t)))

      (setq org-tag-alist
            '(;;places
              ("PRJ" . ?p)
              ("@read" . ?r)
              ("@remind" . ?m)
              ("@email" . ?e)
              ("@idea" . ?i)
              ("@book" . ?b)
              ("@code" . ?c)
              ))

      ;; (setq org-replace-disputed-keys t)
      (setq org-src-fontify-natively t)
      (setq org-return-follows-link t)
      (setq org-startup-folded t)

      (setq org-ditaa-jar-path "~/ownCloud/org/bin/ditaa.jar")
      (setq org-plantuml-jar-path "~/ownCloud/org/bin/plantuml.jar")

      (setq org-contacts-files (list "~/ownCloud/org/roam/areas/agenda/contacts.org"))

      (setq org-journal-dir "~/ownCloud/org/roam/areas/agenda")
      (setq org-journal-date-prefix "* ")
      (setq org-journal-file-type 'monthly)
      (setq org-journal-file-format "%Y%m.org")
      (setq org-journal-date-format "%a, %d %b %Y")
      (setq org-journal-time-format "%Y-%m-%d %I:%M %p")

      (setq org-default-notes-file "~/ownCloud/org/roam/areas/agenda/refile.org")
      (setq org-directory "~/ownCloud/org/roam")

      (setq org-agenda-span 'day)
      (setq org-agenda-files (list "~/ownCloud/org/roam/areas/agenda"))
      (setq org-agenda-persistent-filter t)
      (setq org-agenda-diary-file "~/ownCloud/org/roam/diary.org")
      (setq org-agenda-dim-blocked-tasks nil)
      (setq org-agenda-compact-blocks t)

      ;; Keep tasks with dates on the global todo lists
      (setq org-agenda-todo-ignore-with-date nil)

      ;; Keep tasks with deadlines on the global todo lists
      (setq org-agenda-todo-ignore-deadlines nil)

      ;; Keep tasks with scheduled dates on the global todo lists
      (setq org-agenda-todo-ignore-scheduled nil)

      ;; Keep tasks with timestamps on the global todo lists
      (setq org-agenda-todo-ignore-timestamp nil)

      ;; Remove completed deadline tasks from the agenda view
      (setq org-agenda-skip-deadline-if-done t)

      ;; Remove completed scheduled tasks from the agenda view
      (setq org-agenda-skip-scheduled-if-done t)

      ;; Remove completed items from search results
      (setq org-agenda-skip-timestamp-if-done t)

      ;; Limit restriction lock highlighting to the headline only
      (setq org-agenda-restriction-lock-highlight-subtree nil)

      ;; Include agenda archive files when searching for things
      (setq org-agenda-text-search-extra-files (quote (agenda-archives)))

      ;; Show all future entries for repeating tasks
      (setq org-agenda-repeating-timestamp-show-all t)

      ;; Show all agenda dates - even if they are empty
      (setq org-agenda-show-all-dates t)

      ;; Sorting order for tasks on the agenda
      (setq org-agenda-sorting-strategy
            (quote ((agenda habit-down time-up user-defined-up effort-up category-keep)
                    (todo category-up effort-up)
                    (tags category-up effort-up)
                    (search category-up))))

      ;; Start the weekly agenda on Monday
      (setq org-agenda-start-on-weekday 1)

      ;; Enable display of the time grid so we can see the marker for the current time
      (setq org-agenda-time-grid '((daily today)
                                   (800 1000 1200 1400 1600 1800 2000)
                                   "......" "----------------"))

      ;; Display tags farther right
      (setq org-agenda-tags-column -102)

      ;; Use sticky agenda's so they persist
      (setq org-agenda-sticky t)

      (setq org-agenda-tags-todo-honor-ignore-options t)

      (require 'ox-odt)
      (require 'ox-texinfo)
      (require 'ox-beamer)
      (require 'ox-html)
      (require 'ox-md)

      ;; If idle for more than 15 minutes, resolve the things by asking what to do  with the clock time
      (setq org-clock-idle-time 90)

      ;; log into the LOGBOOK drawer. Also stores notes there.
      (setq org-log-into-drawer t)

      ;; any headline with level <= 2 is a target
      (setq org-refile-targets '((nil :maxlevel . 5)
                                        ; all top-level headlines in the
                                        ; current buffer are used (first) as a
                                        ; refile target
                                 (org-agenda-files :maxlevel . 5)))

      ;; provide refile targets as paths, including the file name
      ;; (without directory) as level 1 of the path
      (setq org-refile-use-outline-path 'file)

      ;; allow to create new nodes (must be confirmed by the user) as refile targets
      (setq org-refile-allow-creating-parent-nodes 'confirm)

      ;; enable helm org refile into subsection of agenda file
      (setq org-outline-path-complete-in-steps nil)

      (add-hook 'org-mode-hook 'org-indent-mode)
      (add-hook 'org-mode-hook 'auto-fill-mode)

      (require 'ox-latex)
      (setq org-latex-listings 'minted)

      ;; setup minted to have frame, small text and line numbers
      (setq org-latex-minted-options
            '(("frame" "lines")
              ("fontsize" "\\scriptsize")
              ("linenos" "")))

      ;; setup org-cdlatex minor mode
      (add-hook 'org-mode-hook 'turn-on-org-cdlatex)

      (setq org-latex-pdf-process (list
                                   "latexmk -pdflatex='%latex -shell-escape -interaction nonstopmode' -pdf -output-directory=%o %f"))

      (setq org-latex-with-hyperref nil)
      (setq org-latex-table-caption-above nil)
      (setq org-html-table-caption-above nil)

      (setq org-export-async-init-file "~/.spacemacs.d/layers/my-org/org-async-init.el")

      ;; remove "inputenc" from default packages as it clashes with xelatex
      ;; (setf org-latex-default-packages-alist
      ;;       (remove '("AUTO" "inputenc" t) org-latex-default-packages-alist))

      ;; Set a nicer default style for the hyperref package
      ;; (setf org-latex-default-packages-alist
      ;;       (remove '("" "hyperref" nil) org-latex-default-packages-alist))

      ;; (add-to-list 'org-latubuntuex-default-packages-alist
      ;;              `("colorlinks=true, linkcolor=teal, urlcolor=teal, citecolor=darkgray, anchorcolor=teal", "hyperref" nil))

      ;; add fontspec package for utf8 characters with xelatex
      ;; (add-to-list 'org-latex-default-packages-alist
      ;;              `("", "fontspec" nil) t)

      (add-to-list 'org-latex-classes
                   '("koma-article"
                     "\\documentclass{scrartcl}"
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                     ("\\paragraph{%s}" . "\\paragraph*{%s}")
                     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

      (add-to-list 'org-latex-classes
                   '("koma-report"
                     "\\documentclass{scrreprt}"
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                     ("\\paragraph{%s}" . "\\paragraph*{%s}")
                     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

      (org-babel-do-load-languages
       'org-babel-load-languages
       '((python . t)
         (ditaa . t)
         (plantuml . t)
         (dot . t)
         (ruby . t)
         (R . t)
         (gnuplot . t)
         (latex . t))
       )

      (defun my-beamer-bold (contents backend info)
        (if (not (eq backend 'beamer)) contents
          (replace-regexp-in-string "\\`\\\\[A-Za-z0-9]+" "\\\\textbf" contents)))

      (defun my-beamer-structure (contents backend info)
        (if (not (eq backend 'beamer)) contents
          (replace-regexp-in-string "\\`\\\\[A-Za-z0-9]+" "\\\\structure" contents)))

      (add-to-list 'org-export-filter-bold-functions 'my-beamer-bold)
      (add-to-list 'org-export-filter-strike-through-functions 'my-beamer-structure)

      (setq org-confirm-babel-evaluate nil)

      (setq org-todo-keywords
            (quote ((sequence "TODO(t)" "INPR(i)" "|" "DONE(d)")
                    (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "CNCL(c@/!)"))))

      (setq org-todo-keyword-faces
            (quote (("TODO" :foreground "#f36c60" :weight bold)
                    ("DONE" :foreground "#8fb573" :weight bold)
                    ("INPR" :foreground "#aaaaff" :weight bold)
                    ("WAIT" :foreground "#dbb671" :weight bold)
                    ("HOLD" :foreground "#70c2be" :weight bold)
                    ("CNCL" :foreground "#8fb573" :weight bold))))

      ;; (setq org-enforce-todo-dependencies t)
      (setq org-use-fast-todo-selection t)

      ;; if state is changed using shift then no dates or notes are recorded
      (setq org-treat-S-cursor-todo-selection-as-state-change nil)

      (setq org-tags-exclude-from-inheritance '("PRJ"))
      (setq org-stuck-projects '("+PRJ/-DONE-CNCL" ("TODO" "WAIT" "HOLD") ()))

      ;; Diary
      (require 'holidays)
      (setq holiday-austria-holidays '((holiday-fixed  1  1 "Neujahr")
                                       (holiday-fixed  1  6 "Heilige Drei Könige")
                                       (holiday-easter-etc 1 "Ostermontag")
                                       (holiday-fixed  5  1 "Staatsfeiertag")
                                       (holiday-easter-etc 39 "Christi Himmelfahrt")
                                       (holiday-easter-etc 50 "Pfingstmontag")
                                       (holiday-easter-etc 60 "Fronleichnam")
                                       (holiday-fixed  8 15 "Mariä Himmelfahrt")
                                       (holiday-fixed 10 26 "Nationalfeiertag")
                                       (holiday-fixed 11  1 "Allerheiligen")
                                       (holiday-fixed 12  8 "Maria Empfängnis")
                                       (holiday-fixed 12 24 "Heilig Abend")
                                       (holiday-fixed 12 25 "Weihnachten")
                                       (holiday-fixed 12 26 "Stefanitag")))

      (setq holiday-local-holidays holiday-austria-holidays)
      (setq calendar-holidays (append holiday-local-holidays holiday-other-holidays))

      (defun my/org-capture-journal-find-month ()
        "Find the month for the Journal entry."
        (let* ((journal-heading "* Journal")
               (month (format-time-string "%B"))
               (month-heading (format "** %s" month)))
          (goto-char (point-min))
          (if (re-search-forward journal-heading nil t)
              (progn
                (unless (re-search-forward month-heading nil t)
                  (goto-char (point-max))
                  (insert (format "%s\n" month-heading))))
            (goto-char (point-max))
            (insert (format "%s\n%s\n" journal-heading month-heading)))
          (re-search-backward month-heading nil t))
        (point))

      (setq org-capture-templates
            (quote (("t" "todo+clock in" entry (file "~/ownCloud/org/roam/areas/agenda/refile.org")
                     "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
                    ("T" "todo" entry (file "~/ownCloud/org/roam/areas/agenda/refile.org")
                     "* TODO %?\n%U\n%a\n")
                    ("n" "note" entry (file "~/ownCloud/org/roam/areas/agenda/refile.org")
                     "* %? :NOTE:\n%U\n%a\n")
                    ("r" "remind" entry (file "~/ownCloud/org/roam/areas/agenda/refile.org")
                     "* %? :RMD:\n%U\n%a\n")
                    ("h" "hsaf journal" entry (file+olp+datetree "~/ownCloud/org/roam/areas/agenda/hsaf.org" "Diary")
                     "* %?\n%U\n")
                    ("j" "journal+clock in" entry (file+olp+datetree "~/ownCloud/org/roam/areas/agenda/diary.org")
                     "* %?\n%U\n" :clock-in t :clock-resume t)
                    ("J" "journal" entry (file+olp+datetree "~/ownCloud/org/roam/areas/agenda/diary.org")
                     "* %?\n%U\n")
                    ("d" "all day journal" entry (file+olp+datetree "~/ownCloud/org/roam/areas/agenda/diary.org")
                     "* %?\n%t\n")
                    ("y" "yearly-journal" entry (file+function "~/ownCloud/org/roam/areas/agenda/2024.org" my/org-capture-journal-find-month)
                     "** %U\n %?")
                    ;; ("e" "event" entry (file "~/ownCloud/org/roam/areas/agenda/calender.org")
                    ;;  "* %^{Description}\n%^t\n%?")
                    ("m" "meeting" entry (file "~/ownCloud/org/roam/areas/agenda/refile.org")
                     "* %? :MEETING:\n%U" :clock-in t :clock-resume t)
                    ("p" "phone call" entry (file "~/ownCloud/org/roam/areas/agenda/refile.org")
                     "* %? :PHONE:\n%U" :clock-in t :clock-resume t)
                    ("a" "habit" entry (file "~/ownCloud/org/roam/areas/agenda/refile.org")
                     "* TODO %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: TODO\n:END:\n"))))


      (setq org-modules (quote (ol-bibtex
                                org-crypt
                                ol-gnus
                                org-id
                                ol-info
                                org-habit
                                org-inlinetask
                                org-protocol
                                )))

      (require 'org-habit)

      (setq org-show-entry-below (quote ((default))))


      ;; Always highlight the current agenda line
      (add-hook 'org-agenda-mode-hook
                #'(lambda () (hl-line-mode 1))
                'append)

      (setq org-archive-mark-done nil)
      (setq org-archive-location "~/ownCloud/org/roam/areas/agenda/archive/%s_archive::* Archived Tasks")

      ;; (setq org-agenda-start-with-log-mode '(clock))

      ;; Priority settings: A, B, C, D
      (setq org-highest-priority ?A)
      (setq org-lowerst-priority ?D)
      (setq org-lowest-priority ?D)
      (setq org-default-priority ?D)

      ;; Switch entry to DONE when all subentries are done, to TODO otherwise.
      ;; (defun org-summary-todo (n-done n-not-done)
      ;;   "Switch entry to DONE when all subentries are done, to TODO otherwise."
      ;;   (let (org-log-done org-log-states)   ; turn off logging
      ;;     (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

      ;; (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

      (setq org-tags-exclude-from-inheritance (quote ("PRJ")))

      ;; Custom agenda command definitions
      (setq org-agenda-custom-commands
            (quote (("N" "Notes" tags ""
                     ((org-agenda-overriding-header "Notes")
                      (org-tags-match-list-sublevels t)))

                    ("r" "Reminders" tags "+RMD"
                     ((org-agenda-view-columns-initially t)
                      (org-agenda-overriding-header "")
                      (org-overriding-columns-format "%80ITEM 20%TIMESTAMP_IA")
                      (org-tags-match-list-sublevels t)))

                    ("d" "Upcoming deadlines" agenda ""
                     ((org-agenda-overriding-header "Upcoming deadlines")
                      (org-agenda-span 'month)
                      (org-agenda-time-grid nil)
                      (org-deadline-warning-days 0)
                      (org-agenda-entry-types '(:deadline))))

                    ("e" "Eisenhower matrix"
                     ((agenda ""
                              ((org-agenda-show-log t)
                               (org-agenda-log-mode-items '(clock closed state))))
                      (tags "+PRJ+PRIORITY=\"A\"-TODO=\"DONE\""
                            ((org-agenda-overriding-header "Urgent and Important (kitchen fire)")
                             (org-agenda-todo-list-sublevels nil)
                             (org-agenda-sorting-strategy '(category-keep))))
                      (tags "+PRJ+PRIORITY=\"B\"-TODO=\"DONE\""
                            ((org-agenda-overriding-header "Important (find date, personally)")
                             (org-agenda-todo-list-sublevels nil)
                             (org-agenda-sorting-strategy '(category-keep))))
                      (tags "+PRJ+PRIORITY=\"C\"-TODO=\"DONE\""
                            ((org-agenda-overriding-header "Urgent (interruptions, delegate)")
                             (org-agenda-todo-list-sublevels nil)
                             (org-agenda-sorting-strategy '(category-keep))))
                      (tags "+PRJ+PRIORITY=\"D\"-TODO=\"DONE\""
                            ((org-agenda-overriding-header "Time waster")
                             (org-agenda-todo-list-sublevels nil)
                             (org-agenda-sorting-strategy '(category-keep))))))

                    ("h" "Habits" tags-todo ""
                     ((org-agenda-overriding-header "Habits")
                      (org-agenda-sorting-strategy
                       '(todo-state-down effort-up category-keep))))

                    ("t" "Kanban agenda (tasks only)"
                     ((agenda ""
                              ((org-agenda-show-log t)
                               (org-agenda-log-mode-items '(clock closed state))))
                      (tags "REFILE"
                            ((org-agenda-overriding-header "Stuff to refile")
                             (org-tags-match-list-sublevels nil)))
                      (tags "-PRJ/TODO"
                            ((org-agenda-overriding-header "Todo")
                             (org-agenda-todo-list-sublevels nil)
                             (org-agenda-sorting-strategy '(priority-down))))
                      (tags "-PRJ/INPR"
                            ((org-agenda-overriding-header "In progress")
                             (org-agenda-todo-list-sublevels nil)
                             (org-agenda-sorting-strategy '(priority-down))))
                      (tags "-PRJ/WAIT"
                            ((org-agenda-overriding-header "Waiting")
                             (org-agenda-todo-list-sublevels nil)
                             (org-agenda-sorting-strategy '(priority-down)))
                            (tags "-PRJ/HOLD"
                                  ((org-agenda-overriding-header "On hold")
                                   (org-agenda-todo-list-sublevels nil)
                                   (org-agenda-sorting-strategy '(priority-down)))))))

                    ("k" "Kanban agenda (projects only)"
                     ((agenda ""
                              ((org-agenda-show-log t)
                               (org-agenda-log-mode-items '(clock))))
                      (tags "REFILE"
                            ((org-agenda-overriding-header "Stuff to refile")
                             (org-agenda-prefix-format "  %?-12t% s")
                             (org-tags-match-list-sublevels nil)))
                      (tags "PRJ/TODO"
                            ((org-agenda-overriding-header "Todo")
                             (org-agenda-todo-list-sublevels nil)
                             (org-agenda-prefix-format "  %?-12t% s")
                             (org-agenda-sorting-strategy '(priority-down))))
                      (tags "PRJ/INPR"
                            ((org-agenda-overriding-header "In progress")
                             (org-agenda-todo-list-sublevels nil)
                             (org-agenda-prefix-format "  %?-12t% s")
                             (org-agenda-sorting-strategy '(priority-down))))
                      (tags "PRJ/WAIT"
                            ((org-agenda-overriding-header "Waiting")
                             (org-agenda-todo-list-sublevels nil)
                             (org-agenda-prefix-format "  %?-12t% s")
                             (org-agenda-sorting-strategy '(priority-down))))
                      (tags "PRJ/HOLD"
                            ((org-agenda-overriding-header "On hold")
                             (org-agenda-todo-list-sublevels nil)
                             (org-agenda-prefix-format "  %?-12t% s")
                             (org-agenda-sorting-strategy '(priority-down))))))

                    ("K" "Kanban agenda (projects + tasks)"
                     ((agenda ""
                              ((org-agenda-show-log t)
                               (org-agenda-log-mode-items '(clock closed state))))
                      (tags "REFILE"
                            ((org-agenda-overriding-header "Stuff to Refile")
                             (org-tags-match-list-sublevels nil)))
                      (todo "TODO"
                            ((org-agenda-overriding-header "Todo")
                             (org-agenda-todo-list-sublevels nil)
                             (org-agenda-sorting-strategy '(priority-down))))
                      (todo "INPR"
                            ((org-agenda-overriding-header "In progress")
                             (org-agenda-todo-list-sublevels nil)
                             (org-agenda-sorting-strategy '(priority-down))))
                      (todo "WAIT"
                            ((org-agenda-overriding-header "Waiting")
                             (org-agenda-todo-list-sublevels nil)
                             (org-agenda-sorting-strategy '(priority-down))))
                      (todo "HOLD"
                            ((org-agenda-overriding-header "On hold")
                             (org-agenda-todo-list-sublevels nil)
                             (org-agenda-sorting-strategy '(priority-down))))))
                    )))

      ;; Resume clocking task when emacs is restarted
      (org-clock-persistence-insinuate)

      ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
      (setq org-clock-history-length 23)

      ;; Resume clocking task on clock-in if the clock is open
      (setq org-clock-in-resume t)

      ;; Change tasks to "in progress" when clocking in
      ;; (setq org-clock-in-switch-to-state "INPR")

      ;; Separate drawers for clocking and logs
      (setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))

      ;; Save clock data and state changes and notes in the LOGBOOK drawer
      (setq org-clock-into-drawer t)

      ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
      (setq org-clock-out-remove-zero-time-clocks t)

      ;; Clock out when moving task to a done state
      (setq org-clock-out-when-done t)

      ;; Save the running clock and all clock history when exiting Emacs, load it on startup
      (setq org-clock-persist t)

      ;; Do not prompt to resume an active clock
      (setq org-clock-persist-query-resume nil)

      ;; Enable auto clock resolution for finding open clocks
      (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))

      ;; Include current clocking task in clock reports
      (setq org-clock-report-include-clocking-task t)

      (setq org-time-stamp-rounding-minutes (quote (1 1)))

      (setq org-agenda-clock-consistency-checks
            (quote (:max-duration "1:00"
                                  :min-duration 0
                                  :max-gap 0
                                  :gap-ok-around ("1:00"))))

      ;; Agenda clock report parameters
      (setq org-agenda-clockreport-parameter-plist
            (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))
      )))
