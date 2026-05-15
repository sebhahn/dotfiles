;;; packages.el --- my-org layer packages file for Spacemacs.
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

;;; Code:

(defconst my-org-packages
  '((org :location built-in)
    org-noter
    ox-typst))

(defun my-org/init-ox-typst ()
  (use-package ox-typst
    :defer t
    :config
    (setq org-typst-process "/home/shahn/.cargo/bin/typst c \"%s\"")))

(defun my-org/init-org-noter ()
  (use-package org-noter
    :defer t))

(defun my-org/pre-init-org ()
  (use-package org
    :defer t
    :commands (org-mode
               org-edit-src-exit
               org-agenda
               org-capture
               org-toggle-latex-fragment
               org-store-link
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
        "ojj" 'org-journal-new-entry
        "ojk" 'org-journal-new-scheduled-entry
        "ojs" 'org-journal-search
        "ojn" 'org-journal-next-entry
        "ojd" 'org-journal-display-entry
        "ojp" 'org-journal-previous-entry
        "ojf" 'org-journal-open-current-journal-file
        "oTh" 'org-toggle-heading)

      (spacemacs/declare-prefix "oa" "org-agenda")
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
        "t" 'spacemacs/org-agenda-transient-state/body
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
        "jj" 'org-journal-new-entry
        "jk" 'org-journal-new-scheduled-entry
        "js" 'org-journal-search
        "jn" 'org-journal-next-entry
        "jp" 'org-journal-previous-entry
        "jf" 'org-journal-open-current-journal-file)

      (spacemacs/set-leader-keys
        "oS" 'org-download-screenshot
        "oC" 'org-download-clipboard
        "oY" 'org-download-yank)

      (spacemacs/set-leader-keys-for-major-mode 'org-mode "C-l" 'org-toggle-latex-fragment))

    :post-config
    (progn

      (defun org-insert-subheading-respect-content ()
        (interactive)
        (let ((org-insert-heading-respect-content t))
          (org-insert-subheading t)))

      (setq org-tag-alist
            '(("PRJ" . ?p)
              ("NOTE" . ?n)
              ("BIBNOTE" . ?b)
              ("REMEMBER" . ?r)))

      (setq org-src-fontify-natively t)
      (setq org-return-follows-link t)
      (setq org-startup-folded t)

      (setq org-ditaa-jar-path "~/ownCloud/org/bin/ditaa.jar")
      (setq org-plantuml-jar-path "~/ownCloud/org/bin/plantuml.jar")

      (setq org-contacts-files (list "~/ownCloud/org/roam/areas/agenda/contacts.org"))

      (setq org-journal-dir "~/ownCloud/org/roam/areas/agenda")
      (setq org-journal-file-type 'weekly)
      (setq org-journal-file-format "%Y-W%V.org")
      (setq org-journal-date-prefix "* ")
      (setq org-journal-date-format "%A, %d %B %Y")
      (setq org-journal-time-prefix "")
      (setq org-journal-time-format "")
      (setq org-journal-file-header #'my/org-journal-weekly-header)

      (setq org-default-notes-file "~/ownCloud/org/roam/areas/agenda/refile.org")
      (setq org-directory "~/ownCloud/org/roam")

      (setq org-agenda-span 'day)
      (setq org-agenda-files (list "~/ownCloud/org/roam/areas/agenda"))
      (setq org-agenda-persistent-filter t)
      (setq org-agenda-diary-file "~/ownCloud/org/roam/diary.org")
      (setq org-agenda-dim-blocked-tasks nil)
      (setq org-agenda-compact-blocks t)
      (setq org-agenda-todo-ignore-with-date nil)
      (setq org-agenda-todo-ignore-deadlines nil)
      (setq org-agenda-todo-ignore-scheduled nil)
      (setq org-agenda-todo-ignore-timestamp nil)
      (setq org-agenda-skip-deadline-if-done t)
      (setq org-agenda-skip-scheduled-if-done t)
      (setq org-agenda-skip-timestamp-if-done t)
      (setq org-agenda-restriction-lock-highlight-subtree nil)
      (setq org-agenda-text-search-extra-files '(agenda-archives))
      (setq org-agenda-repeating-timestamp-show-all t)
      (setq org-agenda-show-all-dates t)
      (setq org-agenda-sorting-strategy
            '((agenda habit-down time-up effort-up category-keep)
              (todo category-up effort-up)
              (tags category-up effort-up)
              (search category-up)))
      (setq org-agenda-start-on-weekday 1)
      (setq org-agenda-time-grid '((daily today)
                                   (0800 1000 1200 1400 1600 1800 2000)
                                   "......" "----------------"))
      (setq org-agenda-tags-column -102)
      (setq org-agenda-sticky t)
      (setq org-agenda-tags-todo-honor-ignore-options t)

      (require 'ox-odt)
      (require 'ox-texinfo)
      (require 'ox-beamer)
      (require 'ox-extra)
      (require 'ox-html)
      (require 'ox-md)

      (ox-extras-activate '(ignore-headlines))

      (setq org-clock-idle-time 90)
      (setq org-log-into-drawer t)

      (setq org-refile-targets '((nil :maxlevel . 5)
                                 (org-agenda-files :maxlevel . 5)))
      (setq org-refile-use-outline-path 'file)
      (setq org-refile-allow-creating-parent-nodes 'confirm)
      (setq org-outline-path-complete-in-steps nil)

      (add-hook 'org-mode-hook 'org-indent-mode)
      (add-hook 'org-mode-hook 'auto-fill-mode)

      (require 'ox-latex)
      (setq org-latex-listings 'minted)
      (add-hook 'org-mode-hook 'turn-on-org-cdlatex)

      (setq org-latex-pdf-process
            '("latexmk -pdflatex='%latex -shell-escape -interaction nonstopmode' -pdf -output-directory=%o %f"))
      (setq org-latex-with-hyperref nil)
      (setq org-latex-table-caption-above nil)
      (setq org-html-table-caption-above nil)

      (setq org-export-async-init-file "~/.spacemacs.d/layers/my-org/org-async-init.el")

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
         (mermaid . t)
         (dot . t)
         (ruby . t)
         (R . t)
         (gnuplot . t)
         (latex . t)))

      (defun my-beamer-bold (contents backend info)
        (when (eq backend 'beamer)
          (replace-regexp-in-string "\\`\\\\[A-Za-z0-9]+" "\\\\textbf" contents)))

      (defun my-beamer-structure (contents backend info)
        (when (eq backend 'beamer)
          (replace-regexp-in-string "\\`\\\\[A-Za-z0-9]+" "\\\\structure" contents)))

      (add-to-list 'org-export-filter-bold-functions 'my-beamer-bold)
      (add-to-list 'org-export-filter-strike-through-functions 'my-beamer-structure)

      (setq org-confirm-babel-evaluate nil)

      (setq org-todo-keywords
            '((sequence "TODO(t)" "INPR(i)" "|" "DONE(d)")
              (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "CNCL(c@/!)")))

      (setq org-todo-keyword-faces
            '(("TODO" :foreground "#f36c60" :weight bold)
              ("DONE" :foreground "#8fb573" :weight bold)
              ("INPR" :foreground "#aaaaff" :weight bold)
              ("WAIT" :foreground "#dbb671" :weight bold)
              ("HOLD" :foreground "#70c2be" :weight bold)
              ("CNCL" :foreground "#8fb573" :weight bold)))

      (setq org-use-fast-todo-selection t)
      (setq org-treat-S-cursor-todo-selection-as-state-change nil)
      (setq org-tags-exclude-from-inheritance '("PRJ"))
      (setq org-stuck-projects '("+PRJ/-DONE-CNCL" ("TODO" "WAIT" "HOLD") ()))

      (require 'holidays)
      (setq holiday-austria-holidays
            '((holiday-fixed  1  1 "Neujahr")
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
        "Find the month heading for a journal entry, creating it if absent."
        (let* ((journal-heading "* Journal")
               (month (format-time-string "%B"))
               (month-heading (format "** %s" month)))
          (goto-char (point-min))
          (if (re-search-forward journal-heading nil t)
              (unless (re-search-forward month-heading nil t)
                (goto-char (point-max))
                (insert (format "%s\n" month-heading)))
            (goto-char (point-max))
            (insert (format "%s\n%s\n" journal-heading month-heading)))
          (re-search-backward month-heading nil t))
        (point))

      (setq org-capture-templates
            '(("t" "todo+clock in" entry (file "~/ownCloud/org/roam/areas/agenda/refile.org")
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
              ("y" "yearly-journal"
               entry
               (file+function
                (lambda ()
                  (expand-file-name
                   (format "%s.org" (format-time-string "%Y"))
                   "~/ownCloud/org/roam/areas/agenda/"))
                my/org-capture-journal-find-month)
               "** %U\n %?")
              ("b" "bibliographic note" entry (file "~/ownCloud/org/roam/areas/agenda/refile.org")
               "* %? :BIBNOTE:\n%U\n%a\n")
              ("c" "add org contact" entry (file "~/ownCloud/org/roam/areas/agenda/contacts.org")
               "* %(org-contacts-template-name)")
              ("m" "meeting" entry (file "~/ownCloud/org/roam/areas/agenda/refile.org")
               "* %? :MEETING:\n%U" :clock-in t :clock-resume t)
              ("p" "phone call" entry (file "~/ownCloud/org/roam/areas/agenda/refile.org")
               "* %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("a" "habit" entry (file "~/ownCloud/org/roam/areas/agenda/refile.org")
               "* TODO %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: TODO\n:END:\n")))

      (setq org-modules '(ol-bibtex
                          org-crypt
                          ol-gnus
                          org-id
                          ol-info
                          org-habit
                          org-inlinetask
                          org-protocol))

      (require 'org-habit)

      (add-hook 'org-agenda-mode-hook
                (lambda () (hl-line-mode 1))
                'append)

      (setq org-archive-mark-done nil)
      (setq org-archive-location "~/ownCloud/org/roam/areas/agenda/archive/%s_archive::* Archived Tasks")

      (setq org-highest-priority ?A)
      (setq org-lowest-priority ?D)
      (setq org-default-priority ?D)

      (setq org-agenda-custom-commands
            '(("N" "Notes" tags ""
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
              ("w" "Weekly Review"
               ((org-agenda-span 7)
                (org-deadline-warning-days 0)))
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
                (org-agenda-sorting-strategy '(todo-state-down effort-up category-keep))))
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
                       (org-agenda-sorting-strategy '(priority-down))))
                (tags "-PRJ/HOLD"
                      ((org-agenda-overriding-header "On hold")
                       (org-agenda-todo-list-sublevels nil)
                       (org-agenda-sorting-strategy '(priority-down))))))
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
                       (org-agenda-sorting-strategy '(priority-down))))))))

      (org-clock-persistence-insinuate)
      (setq org-clock-history-length 23)
      (setq org-clock-in-resume t)
      (setq org-clock-into-drawer t)
      (setq org-clock-out-remove-zero-time-clocks t)
      (setq org-clock-out-when-done t)
      (setq org-clock-persist t)
      (setq org-clock-persist-query-resume nil)
      (setq org-clock-auto-clock-resolution 'when-no-clock-is-running)
      (setq org-clock-report-include-clocking-task t)
      (setq org-time-stamp-rounding-minutes '(1 1))
      (setq org-agenda-clock-consistency-checks
            '(:max-duration "1:00"
              :min-duration 0
              :max-gap 0
              :gap-ok-around ("1:00")))
      (setq org-agenda-clockreport-parameter-plist
            '(:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))))
