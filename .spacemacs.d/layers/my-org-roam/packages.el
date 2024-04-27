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
    org-roam-bibtex
    citar
    citar-embark
    citar-org-roam))

(defun my-org-roam/init-org-roam ()
  (use-package org-roam
    :after (org)
    :init
    (setq org-roam-v2-ack t)
    (progn
      (spacemacs/declare-prefix "or" "org-roam")
      (spacemacs/declare-prefix "org" "org-roam-dailies-goto")
      (spacemacs/declare-prefix "orr" "org-roam-refs")
      (spacemacs/set-leader-keys
        "orl" 'org-roam-buffer-toggle
        "ordc" 'org-roam-dailies-capture-today
        "ordp" 'org-roam-dailies-goto-yesterday
        "ord." 'org-roam-dailies-goto-today
        "ordn" 'org-roam-dailies-goto-tomorrow
        "orgd" 'org-roam-dailies-goto-date
        "orgp" 'org-roam-dailies-goto-previous-note
        "orgn" 'org-roam-dailies-goto-next-note
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
        "rdc" 'org-roam-dailies-capture-today
        "rdp" 'org-roam-dailies-goto-yesterday
        "rd." 'org-roam-dailies-goto-today
        "rdn" 'org-roam-dailies-goto-tomorrow
        "rgd" 'org-roam-dailies-goto-date
        "rgp" 'org-roam-dailies-goto-previous-note
        "rgn" 'org-roam-dailies-goto-next-note
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
    (org-roam-dailies-directory "~/ownCloud/org/roam/areas/daily/")
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
          :if-new (file+head "${slug}.org" "#+title: ${title}\n")
          :unnarrowed t)
        ("n" "literature note" plain "%?"
          :target
          (file+head "~/ownCloud/org/roam/resources/publications/${citar-citekey}.org" "#+title: ${title}\n\n")
          :unnarrowed t)))
    (org-roam-db-autosync-mode)
 ))

(defun my-org-roam/init-org-roam-bibtex ()
  (use-package org-roam-bibtex
    :after org-roam
    :hook (org-roam-mode . org-roam-bibtex-mode)
    :bind (:map org-mode-map
                (("C-c n a" . orb-note-actions)))
    :config
    (setq orb-insert-link-description "citation")
    ))

(defun my-org-roam/init-citar ()
  (use-package citar
    :after org
    :custom
    (org-cite-global-bibliography '("~/ownCloud/areas/research/latex/zotero.bib"))
    (org-cite-insert-processor 'citar)
    (org-cite-follow-processor 'citar)
    (org-cite-activate-processor 'citar)
    (citar-bibliography org-cite-global-bibliography)
    (citar-notes-paths '("~/ownCloud/org/roam/resources/publications/"))
    (citar-library-paths '("~/ownCloud/areas/research/publications/"))
    (citar-file-note-extension '("org"))
    (citar-at-point-function 'embark-act)
    (citar-indicators (list citar-indicator-files ; plain text
                            citar-indicator-notes-icons))
    ;; optional: org-cite-insert is also bound to C-c C-x C-@
    :bind
    (:map org-mode-map
          :package org ("C-c b" . #'org-cite-insert))
    :hook
    (LaTeX-mode . citar-capf-setup)
    (org-mode . citar-capf-setup)
    :config
    ;; (add-to-list 'citar-file-open-functions '("pdf" . citar-file-open-external))
    ;; (spacemacs/set-leader-keys "o b" 'citar-open)
    (spacemacs/declare-prefix "ob" "citar")
    (spacemacs/set-leader-keys
      "obo" 'citar-open
      "obi" 'citar-insert-citation
      "obc" 'citar-create-note
      "obd" 'citar-dwim)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "rbo" 'citar-open
      "rbi" 'citar-insert-citation
      "rbc" 'citar-create-note
      "rbd" 'citar-dwim)
    (add-to-list 'citar-file-open-functions '("pdf" . (lambda (fpath) (start-process "zathura" "*zathura*" "/usr/bin/zathura" fpath))))))

(defun my-org-roam/init-citar-embark ()
  (use-package citar-embark
    :after citar embark
    :no-require
    :config (citar-embark-mode)))

(defun my-org-roam/init-citar-org-roam ()
  (use-package citar-org-roam
    :after (citar org-roam)
    :config (citar-org-roam-mode)
    :custom
    (citar-org-roam-note-title-template "${title}")
    (citar-org-roam-capture-template-key "n")))
