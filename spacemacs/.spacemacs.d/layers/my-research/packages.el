;;; packages.el --- my-research layer packages file for Spacemacs.
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
;; added to `my-research-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-research/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-research/pre-init-PACKAGE' and/or
;;   `my-research/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-research-packages
  '(org-ref))

(defun my-research/post-init-org-ref()
  "Init org ref package"
  (use-package org-ref
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys "on" 'org-ref-open-notes-at-point)
      ;; (spacemacs/set-leader-keys-for-major-mode 'org-mode "rN" 'org-ref-open-notes-at-point)
      )
    :config

    (require 'org-ref-bibtex)
    (require 'org-ref-arxiv)
    (require 'org-ref-scopus)
    (require 'bibtex)

    (setq bibtex-completion-bibliography '("~/ownCloud/areas/research/latex/zotero.bib")
          bibtex-completion-library-path '("~/ownCloud/areas/research/publications/")
          bibtex-completion-notes-path "~/ownCloud/org/roam/resources/zettelkasten/bibliography/"
          bibtex-completion-notes-template-multiple-files (format ":properties:\n:id: ${=key=}\n:roam_aliases: ${=key=}\n:roam_refs: cite:${=key=}\n:end:\n#+title: ${title}\n#+author: ${author-abbrev}\n\n")
          bibtex-completion-additional-search-fields '(keywords)
          bibtex-completion-display-formats
          '((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
            (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
            (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
            (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
            (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
          bibtex-completion-pdf-open-function (lambda (fpath)
                                                (start-process "zathura" "*zathura*" "/usr/bin/zathura" fpath)))

    (setq org-ref-open-pdf-function
          (lambda (fpath)
            (start-process "zathura" "*zathura*" "/usr/bin/zathura" fpath)))

    (setq bibtex-autokey-year-length 4
          bibtex-autokey-name-year-separator "-"
          bibtex-autokey-year-title-separator "-"
          bibtex-autokey-titleword-separator "-"
          bibtex-autokey-titlewords 2
          bibtex-autokey-titlewords-stretch 1
          bibtex-autokey-titleword-length 5)

    (define-key bibtex-mode-map (kbd "H-b") 'org-ref-bibtex-hydra/body)
    (define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link)
    (define-key org-mode-map (kbd "s-[") 'org-ref-insert-link-hydra/body)

    (setq reftex-default-bibliography '("~/ownCloud/areas/research/latex/zotero.bib")))
  )
