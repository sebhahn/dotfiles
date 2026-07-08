(defun my/week-start (time)
  "Return TIME of Monday of the ISO week containing TIME."
  (let* ((date (calendar-gregorian-from-absolute
                (calendar-absolute-from-gregorian
                 (list (string-to-number (format-time-string "%m" time))
                       (string-to-number (format-time-string "%d" time))
                       (string-to-number (format-time-string "%Y" time))))))
         (dow (calendar-day-of-week date)))
    (seconds-to-time
     (- (float-time time) (* 86400 (mod (+ dow 6) 7))))))

(defun my/week-end (time)
  "Return TIME of Sunday of the ISO week containing TIME."
  (seconds-to-time (+ (float-time (my/week-start time)) (* 6 86400))))

(defun my/org-journal-weekly-header (time)
  (let* ((year (format-time-string "%G" time))
         (week (format-time-string "%V" time))
         (start (format-time-string "%Y-%m-%d" (my/week-start time)))
         (end   (format-time-string "%Y-%m-%d" (my/week-end time))))
    (format "#+TITLE: Weekly Journal %s-W%s (%s to %s)\n\n"
            year week start end)))

(defun my/org-insert-timestamp-below-heading (&optional active)
  "Insert a timestamp with the current date and time on a new line
directly below the current heading, after any planning lines and
property drawers.  The timestamp is inactive by default; with a
prefix argument (or when ACTIVE is non-nil) insert an active one."
  (interactive "P")
  (org-back-to-heading t)
  (org-end-of-meta-data t)
  (unless (bolp) (insert "\n"))
  (insert (format-time-string (if active "<%Y-%m-%d %a %H:%M>\n"
                                 "[%Y-%m-%d %a %H:%M]\n"))))

(defun my/org-insert-active-timestamp-below-heading ()
  "Like `my/org-insert-timestamp-below-heading' but active."
  (interactive)
  (my/org-insert-timestamp-below-heading t))

(defun my/org-journal-ensure-title (&rest _)
  "Insert #+TITLE if missing in the current org-journal buffer."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^#\\+TITLE:" nil t)
      (goto-char (point-min))
      (insert (my/org-journal-weekly-header (current-time))))))
