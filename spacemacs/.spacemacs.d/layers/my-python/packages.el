;;; packages.el --- my-python layer packages file for Spacemacs.
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
;; added to `my-python-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-python/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-python/pre-init-PACKAGE' and/or
;;   `my-python/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-python-packages
  '(python))

(defun my-python/post-init-python ()
  ;; Interpreter selection is handled by pet (it sets `python-shell-interpreter'
  ;; to the project venv on `python-base-mode-hook'). Do not override it here.
  (advice-add 'python-shell-completion-at-point :around
              (lambda (orig-fun &rest args)
                (condition-case nil
                    (apply orig-fun args)
                  (json-parse-error nil))))
  ;; compilation-start-hook fires inside compilation-start, in the compilation
  ;; buffer, after compilation--start-time is set but before compile returns.
  ;; Save to a plain global so kill-all-local-variables (called by
  ;; inferior-python-mode immediately after compile in spacemacs/python-execute-file)
  ;; cannot wipe it.
  (defun my-python--save-start-time (_proc)
    (setq my-python--last-compile-start-time compilation--start-time))
  (add-hook 'compilation-start-hook #'my-python--save-start-time)

  ;; Named advice: restore the real start time when it was wiped.
  (defun my-python--guard-nil-start-time (&rest _)
    (unless compilation--start-time
      (setq compilation--start-time
            (or my-python--last-compile-start-time (float-time)))))
  (advice-add 'compilation-handle-exit :before
              #'my-python--guard-nil-start-time))

;; (defun my-python/init-company-jedi()
;;   (use-package company-jedi)

;;   (defun my-python/python-mode-hook ()
;;     (add-to-list 'company-backends 'company-jedi))

;;   (add-hook 'python-mode-hook 'my-python/python-mode-hook)
;;   )

;; (defun my-python/post-init-company-anaconda ()
;;   (use-package company-anaconda
;;     :defer t
;;     :init
;;     (spacemacs|add-company-backends
;;       :backends company-anaconda :with company-jedi :with company-yasnippet
;;       :modes python-mode)))
