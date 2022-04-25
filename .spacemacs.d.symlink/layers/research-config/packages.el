;;; packages.el --- research-config Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar research-config-packages
   '(org-ref)
  "List of all packages to install and/or initialize. Built-in packages which require an initialization must be listed explicitly in the list.")

(defvar research-config-excluded-packages '()
  "List of packages to exclude.")

(defun research-config/post-init-org-ref()
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
          bibtex-completion-notes-path "~/ownCloud/org/roam/"
          bibtex-completion-notes-template-multiple-files (format "#+TITLE: ${title}\n#+ROAM_KEY: cite:${=key=}\n\n")

      bibtex-completion-additional-search-fields '(keywords)
      bibtex-completion-pdf-open-function
          (lambda (fpath)
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
	        bibtex-autokey-titleword-length 5
	        org-ref-bibtex-hydra-key-binding (kbd "H-b"))

    (define-key bibtex-mode-map (kbd "H-b") 'org-ref-bibtex-hydra/body)

    (setq reftex-default-bibliography '("~/ownCloud/areas/research/latex/zotero.bib"))
))

;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
