;;; packages.el --- my-television layer packages file for Spacemacs.  -*- lexical-binding: t -*-
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

(defconst my-television-packages '())

(defvar my-television-binary "~/bin/tv"
  "Path to the tv binary.")

(defvar my-television-preview nil
  "When non-nil, show a bat-powered preview panel in tv.")

(defun my-television-toggle-preview ()
  "Toggle the tv preview panel on/off."
  (interactive)
  (setq my-television-preview (not my-television-preview))
  (message "television preview: %s" (if my-television-preview "on" "off")))

(defun my-television--preview-flags ()
  (if my-television-preview
      "--preview-command 'bat --color=always --style=numbers {}'"
    "--no-preview"))

(defun my-television--run (channel dir action)
  "Open tv for CHANNEL in DIR in a wezterm window. Call ACTION with the selected entry."
  (let* ((dir (expand-file-name dir))
         (result-file (make-temp-file "tv-result"))
         (shell-cmd (format "cd %s && %s %s %s > %s"
                            (shell-quote-argument dir)
                            my-television-binary
                            channel
                            (my-television--preview-flags)
                            (shell-quote-argument result-file)))
         (proc (start-process "television" nil
                              "wezterm" "start" "--" "/bin/sh" "-c" shell-cmd)))
    (set-process-sentinel
     proc
     (lambda (_proc event)
       (when (string-prefix-p "finished" event)
         (let ((result (with-temp-buffer
                         (insert-file-contents result-file)
                         (string-trim (buffer-string)))))
           (delete-file result-file)
           (unless (string-empty-p result)
             (run-with-timer 0 nil action (expand-file-name result dir)))))))))

(defun my-television-find-file ()
  "Pick a file with tv and open it."
  (interactive)
  (my-television--run "files" default-directory #'find-file))

(defun my-television-git-log ()
  "Browse git log with tv."
  (interactive)
  (my-television--run "git-log" default-directory
                      (lambda (entry)
                        (magit-show-commit (car (split-string entry))))))

(defun my-television-git-branch ()
  "Switch git branch with tv."
  (interactive)
  (my-television--run "git-branch" default-directory
                      (lambda (branch)
                        (magit-branch-checkout (string-trim branch)))))
