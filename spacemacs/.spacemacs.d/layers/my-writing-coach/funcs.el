;;; funcs.el --- my-writing-coach layer functions file for Spacemacs. -*- lexical-binding: t; -*-

;;; Commentary:

;; Records the paragraph at point (one paragraph per line) to
;; <root>/.writing/trace.jsonl when Emacs is idle, for the writing-coach
;; Claude skill.  <root> is the nearest parent folder with a .writing/ dir.

;;; Code:

(require 'json)

(defvar writing-coach t
  "When nil, do not auto-enable `writing-coach-mode' in this file.
Set it as a file-local or dir-local variable to opt files or folders out.")
(put 'writing-coach 'safe-local-variable #'booleanp)

(defvar writing-coach-modes '(text-mode typst-ts-mode typst-mode)
  "Major modes (and derived modes) where `writing-coach-mode' auto-enables.")

(defvar writing-coach-idle-seconds 20
  "Idle time before the current paragraph is recorded.")

(defvar writing-coach--timer nil)
(defvar-local writing-coach--last-text nil)
(defvar-local writing-coach--last-tick nil)

(defun writing-coach--root ()
  "Folder containing .writing/ for the current file, or nil."
  (and buffer-file-name
       (locate-dominating-file buffer-file-name ".writing")))

(defun writing-coach--snapshot ()
  "Append the paragraph at point to the trace if it was edited."
  (when (and writing-coach-mode buffer-file-name)
    (let ((root (writing-coach--root))
          (tick (buffer-chars-modified-tick))
          (text (string-trim (or (thing-at-point 'line t) ""))))
      (when (and root
                 (not (eql tick writing-coach--last-tick))
                 (not (string-empty-p text))
                 (not (equal text writing-coach--last-text)))
        (setq writing-coach--last-tick tick
              writing-coach--last-text text)
        (let ((create-lockfiles nil))
          (write-region
           (concat (json-encode
                    `((time . ,(format-time-string "%FT%T"))
                      (file . ,(file-relative-name buffer-file-name root))
                      (line . ,(line-number-at-pos))
                      (text . ,text)))
                   "\n")
           nil (expand-file-name ".writing/trace.jsonl" root) 'append 'silent))))))

(define-minor-mode writing-coach-mode
  "Record the paragraph at point for the writing-coach Claude skill."
  :lighter " WC"
  (when writing-coach-mode
    (unless (writing-coach--root)
      (setq writing-coach-mode nil)
      (user-error "writing-coach: no .writing/ folder above this file"))
    (setq writing-coach--last-tick (buffer-chars-modified-tick))
    (unless writing-coach--timer
      (setq writing-coach--timer
            (run-with-idle-timer writing-coach-idle-seconds t
                                 #'writing-coach--snapshot)))))

(defun writing-coach-maybe-enable ()
  "Enable `writing-coach-mode' in writing files of projects with .writing/."
  (when (and buffer-file-name
             writing-coach
             (apply #'derived-mode-p writing-coach-modes)
             (writing-coach--root))
    (writing-coach-mode 1)))

;;; funcs.el ends here
