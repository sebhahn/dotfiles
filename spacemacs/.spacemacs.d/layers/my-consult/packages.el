;;; packages.el --- my-consult layer packages file for Spacemacs.
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
;; added to `my-consult-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-consult/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-consult/pre-init-PACKAGE' and/or
;;   `my-consult/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-consult-packages
  '(consult
    consult-company
    consult-org-roam
    consult-dir
    embark))

(defun my-consult/post-init-consult()
  (spacemacs/set-leader-keys
    "fd" 'consult-fd))

(defun my-consult/init-consult-dir ()
  (use-package consult-dir
    :ensure t
    :bind (:map vertico-map
           ("C-x C-d" . consult-dir)
           ("C-x C-j" . consult-dir-jump-file)))
  )

(defun my-consult/post-init-embark ()
  (global-set-key (kbd "C-.") 'embark-act)
  (spacemacs/set-leader-keys "oe" 'embark-act))

(defun my-consult/init-consult-company()
  (use-package consult-company
    :ensure t
    :defer t
    :config
    (define-key company-mode-map [remap completion-at-point] #'consult-company)))

(defun my-consult/init-consult-org-roam()
  (use-package consult-org-roam
    :ensure t
    :after org-roam
    :custom
    (consult-org-roam-grep-func #'consult-ripgrep)
    :config
    (consult-org-roam-mode 1)
    (spacemacs/set-leader-keys
      "orF" 'consult-org-roam-file-find
      "orb" 'consult-org-roam-backlinks
      "or/" 'consult-org-roam-search)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "rF" 'consult-org-roam-file-find
      "rB" 'consult-org-roam-backlinks
      "r/" 'consult-org-roam-search)))

