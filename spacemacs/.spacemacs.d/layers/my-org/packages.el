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

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `my-org-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-org/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-org/pre-init-PACKAGE' and/or
;;   `my-org/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-org-packages
   '())
;; org-plus-contrib
;;     (org :location built-in)
;;     (org-ac :location built-in)
;;     cdlatex)
;;   )

;; (defun my-org/init-cdlatex()
;;   )

;; (defun my-org/init-org-ac()
;;   (use-package org-ac
;;     :defer t
;;     :config
;;     (progn
;;       (org-ac/config-default)))
;;   )

;; (defun custom-org-config/pre-init-org ()
;;   (use-package org
;;     :defer t
;;     :commands (org-mode
;;                org-edit-src-exit
;;                org-agenda
;;                org-capture
;;                org-toggle-latex-fragment
;;                org-store-link
;;                org-iswitchb
;;                org-clock-goto
;;                org-clock-in))
;;   (spacemacs|use-package-add-hook org
;;     :post-init
;;     )
;;   )
