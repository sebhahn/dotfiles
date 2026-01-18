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
