;;; packages.el --- my-org-roam layer packages file for Spacemacs.
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
;; added to `my-org-roam-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-org-roam/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-org-roam/pre-init-PACKAGE' and/or
;;   `my-org-roam/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-org-roam-packages
  '(org-roam
    org-roam-bibtex)
  "The list of Lisp packages required by the my-org-roam layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun my-org-roam/init-org-roam ()
  (use-package org-roam
    :defer t
    :after (org)
    :init
    (setq org-roam-v2-ack t)
    (progn
      (spacemacs/declare-prefix "or" "org-roam")
      (spacemacs/set-leader-keys
        "orl" 'org-roam-buffer-toggle
        "orp" 'org-roam-dailies-goto-yesterday
        "or." 'org-roam-dailies-goto-today
        "orn" 'org-roam-dailies-goto-tomorrow
        "orw" 'org-roam-node-random
        "orf" 'org-roam-node-find
        "ori" 'org-roam-node-insert
        "orI" 'orb-insert-link
        "orA" 'orb-note-actions
        "org" 'org-roam-graph
        "orc" 'org-roam-capture
        "orra" 'org-roam-ref-add
        "orrf" 'org-roam-ref-find
        "orrr" 'org-roam-ref-remove
        "orta" 'org-roam-tag-add
        "ortd" 'org-roam-tag-remove
        "oraa" 'org-roam-alias-add
        "orad" 'org-roam-alias-remove)

      (spacemacs/declare-prefix-for-mode 'org-mode "r" "org-roam")
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "rl" 'org-roam-buffer-toggle
        "rp" 'org-roam-dailies-goto-yesterday
        "r." 'org-roam-dailies-goto-today
        "rn" 'org-roam-dailies-goto-tomorrow
        "rw" 'org-roam-node-random
        "rf" 'org-roam-node-find
        "ri" 'org-roam-node-insert
        "rI" 'orb-insert-link
        "rA" 'orb-note-actions
        "rg" 'org-roam-graph
        "rc" 'org-roam-capture
        "rra" 'org-roam-ref-add
        "rrf" 'org-roam-ref-find
        "rrr" 'org-roam-ref-remove
        "rta" 'org-roam-tag-add
        "rtr" 'org-roam-tag-remove
        "raa" 'org-roam-alias-add
        "rar" 'org-roam-alias-remove))
    :custom
    (org-roam-directory "~/ownCloud/org/roam/")
    :config
    (progn
      (spacemacs|hide-lighter org-roam-mode)
      (when org-enable-roam-protocol
        (add-hook 'org-roam-mode-hook (lambda ()
                                        (require 'org-roam-protocol))))

      (evilified-state-evilify-map org-roam-mode-map
        :mode org-roam-mode
        :bindings
        "o" 'link-hint-open-link
        "r" 'org-roam-buffer-refresh))

    (setq org-roam-mode-section-functions
          (list #'org-roam-backlinks-section
                #'org-roam-reflinks-section
                ;; #'org-roam-unlinked-references-section
                ))

    (setq org-roam-completion-everywhere t)
    (setq org-roam-file-extensions '("org"))
    (setq org-roam-db-node-include-function
          (lambda () (not (member "ATTACH" (org-get-tags)))))
    (setq org-roam-capture-templates
      '(("d" "default" plain "%?"
        :if-new (file+head "${slug}.org"
                            "#+title: ${title}\n")
        :unnarrowed t)))

    (org-roam-setup)
    ))

(defun my-org-roam/init-org-roam-bibtex ()
  (use-package org-roam-bibtex
    :after org-roam
    :hook (org-roam-mode . org-roam-bibtex-mode)
    :bind (:map org-mode-map
                (("C-c n a" . orb-note-actions)))
    :config
    (setq orb-insert-link-description "citation")
    (require 'org-ref)
    ))
