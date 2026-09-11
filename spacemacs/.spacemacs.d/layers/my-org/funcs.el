;;; -*- lexical-binding: t -*-
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

(defun my/org-agenda-deadline-prefix ()
  "Return the deadline date of the entry at point as an agenda prefix."
  (let ((deadline (org-entry-get nil "DEADLINE")))
    (if deadline (format "%s: " (substring deadline 1 11)) "")))

(defun my/org-agenda-ia-timestamp (&optional entry)
  "Return the inactive timestamp of the entry at point.
With agenda line ENTRY, return the timestamp of the entry it points to.
Return the empty string when there is none."
  (let ((marker (and entry (get-text-property 0 'org-marker entry))))
    (or (if marker
            (org-with-point-at marker (org-entry-get nil "TIMESTAMP_IA"))
          (org-entry-get nil "TIMESTAMP_IA"))
        "")))

(defun my/org-agenda-ia-timestamp-prefix ()
  "Return the inactive timestamp of the entry at point as an agenda prefix."
  (let ((timestamp (my/org-agenda-ia-timestamp)))
    (if (equal timestamp "") "            "
      (format "%s: " (substring timestamp 1 11)))))

(defun my/org-agenda-cmp-ia-timestamp (a b)
  "Compare agenda entries A and B by their inactive timestamps.
`ts-up' only sorts active timestamps, so reminders need this."
  (let ((ta (my/org-agenda-ia-timestamp a))
        (tb (my/org-agenda-ia-timestamp b)))
    (cond ((string< ta tb) -1)
          ((string< tb ta) +1))))

(defun my/org-clock-in-switch-to-inpr (state)
  "Switch a task from TODO or HOLD to INPR when clocking in.
Clocking a held task is what lifts it off hold again.  WAIT and the
done states are left alone, so clocking in on a task waiting for
someone does not make it look like it is being worked on.  STATE is
the current todo keyword."
  (when (member state '("TODO" "HOLD")) "INPR"))

(defun my/org-clock-in-switch-project-to-inpr ()
  "Switch the projects containing the clocked task to INPR.
A project is a headline tagged PRJ.  Every enclosing one that is TODO,
WAIT or HOLD is switched, so clocking a task in a nested project marks
the whole chain as being worked on, and resuming work on a parked
project brings it back.  Clocking a WAIT task does not change that
task itself, only the projects above it."
  (when (marker-buffer org-clock-marker)
    (org-with-point-at org-clock-marker
      (while (org-up-heading-safe)
        (when (and (member "PRJ" (org-get-tags nil t))
                   (member (org-get-todo-state) '("TODO" "WAIT" "HOLD")))
          (org-todo "INPR"))))))

(add-hook 'org-clock-in-hook #'my/org-clock-in-switch-project-to-inpr)

(defun my/org-clocking-entry-p ()
  "Non-nil when the entry at point is the one currently being clocked."
  (and (marker-buffer org-clock-marker)
       (eq (marker-buffer org-clock-marker) (current-buffer))
       (= (save-excursion (org-back-to-heading t) (point))
          (org-with-point-at org-clock-marker
            (org-back-to-heading t)
            (point)))))

(defun my/org-subtree-todo-states ()
  "Return the todo keywords below the entry at point.
The whole subtree is scanned, not just the direct children, so a task
buried under a plain heading still counts as work on the project."
  (let ((states nil)
        (end (save-excursion (org-end-of-subtree t t) (point))))
    (save-excursion
      (org-back-to-heading t)
      (while (and (outline-next-heading) (< (point) end))
        (push (org-get-todo-state) states)))
    (delq nil states)))

(defun my/org-park-blocked-projects ()
  "Park a project when none of its subtasks can be worked on.
A project with a WAIT subtask left is set to WAIT, one whose subtasks
are all DONE or CNCL is set to HOLD, so a project that ran out of work
while waiting for someone is parked once that wait ends too.  Only
active projects are switched, and a project being clocked right now is
left alone.  The note prompt these states normally trigger is
suppressed, so closing a subtask never interrupts."
  (save-excursion
    (while (org-up-heading-safe)
      (when (and (member "PRJ" (org-get-tags nil t))
                 (member (org-get-todo-state) '("TODO" "INPR" "WAIT"))
                 (not (my/org-clocking-entry-p)))
        (let ((states (my/org-subtree-todo-states)))
          (when (and states
                     (not (seq-some (lambda (state)
                                      (member state '("TODO" "INPR")))
                                    states)))
            (let ((target (if (member "WAIT" states) "WAIT" "HOLD"))
                  (org-inhibit-logging 'note))
              (unless (equal target (org-get-todo-state))
                (org-todo target)))))))))

(add-hook 'org-after-todo-state-change-hook #'my/org-park-blocked-projects)

(defun my/org-unpark-revived-projects ()
  "Bring a parked project back when it has work to do again.
Every HOLD or WAIT project above the entry at point is set to INPR when
one of its subtasks is being worked on and to TODO when one is merely
actionable, so reopening a task revives the project the way clocking one
in does.  Projects finished by hand are left alone, and the note prompt
leaving these states normally triggers is suppressed."
  (save-excursion
    (while (org-up-heading-safe)
      (when (and (member "PRJ" (org-get-tags nil t))
                 (member (org-get-todo-state) '("WAIT" "HOLD")))
        (let ((states (my/org-subtree-todo-states))
              (org-inhibit-logging 'note))
          (cond ((member "INPR" states) (org-todo "INPR"))
                ((member "TODO" states) (org-todo "TODO"))))))))

(add-hook 'org-after-todo-state-change-hook #'my/org-unpark-revived-projects)
