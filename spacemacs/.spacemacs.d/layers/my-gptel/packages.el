;;; packages.el --- my-gptel layer packages file for Spacemacs.  -*- lexical-binding: t -*-
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

(defconst my-gptel-packages
  '(gptel gptel-magit))

(defconst my-gptel-commit-prompt
  "You are an expert programmer writing a commit message.
You went over every file diff that was changed in it.

First Determine the best label for the diffs.

Here are the labels you can choose from:
- feat: a commit of the type feat introduces a new feature to the codebase (a new feature for the user, not a new feature for build script)
- fix: A commit of the type fix patches a bug in your codebase (a bug fix for the user, not a fix to a build script)
- docs: Changes to the documentation
- style: Changes that do not affect the meaning of the code (white-space, formatting, missing semi-colons, etc)
- refactor: A code change that neither fixes a bug nor adds a feature, eg. renaming a variable
- perf: A code change that improves performance
- test: Adding missing tests, correcting or refactoring existing tests
- chore: Updating libraries, copyrights or other repo setting, includes updating dependencies.
- build: Changes that affect the build system or external dependencies (example scopes: gulp, broccoli, npm)
- ci: Changes to our CI configuration files and scripts (example scopes: Travis, Circle, GitHub Actions)

Then summarize the commit into a single specific and cohesive theme.
Remember to write in only one line, no more than 50 characters.
Write your response using the imperative tense following the kernel git commit style guide.
Write a high level title.

Now write a Commit message in the following template with no additional commentary or formatting:
[label]:[one line of summary]
")

(defun my-gptel//geoforge-backend (name &rest params)
  "Register the geoforge Open WebUI endpoint as NAME, with extra PARAMS."
  (apply #'gptel-make-openai name
         :host "forge.geo.tuwien.ac.at"
         :endpoint "/api/v1/chat/completions"
         :stream t
         :models '(qwen gemma4)
         :key (lambda ()
                (or (auth-source-pick-first-password :host "geoforge")
                    (user-error "No auth-source entry for host \"geoforge\"")))
         params))

(defun my-gptel/post-init-gptel ()
  (with-eval-after-load 'gptel
    (setq gptel-backend (my-gptel//geoforge-backend "geoforge")
          gptel-model 'qwen)))

(defun my-gptel/init-gptel-magit ()
  (use-package gptel-magit
    :after magit
    :config
    (setq gptel-magit-commit-prompt my-gptel-commit-prompt)
    (setq gptel-magit-backend
          (my-gptel//geoforge-backend "geoforge-nothink"
                                      :request-params '(:reasoning_effort "none")))
    (gptel-magit-install)))
