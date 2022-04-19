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
   '(helm-bibtex
     org-ref)
  "List of all packages to install and/or initialize. Built-in packages which require an initialization must be listed explicitly in the list.")

(defvar research-config-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function research-config/init-<package-research-config>
;;
(defun research-config/post-init-helm-bibtex()
   "Initialize my package"
   (use-package helm-bibtex
     :defer t
     :commands helm-bibtex
     :init
     (progn
       (evil-leader/set-key "ob" 'helm-bibtex)
       (evil-leader/set-key "oB" 'helm-bibtex-with-notes)
       )
     :config
     (progn

   (setq helm-bibtex-library-path "~/ownCloud/areas/research/publications/")
   (setq helm-bibtex-bibliography "~/ownCloud/areas/research/latex/zotero.bib")
   (setq helm-bibtex-notes-path "~/ownCloud/org/roam")
   (setq helm-bibtex-pdf-open-function
     (lambda (fpath)
       (start-process "okular" "*okular*" "okular" fpath)))
   (setq helm-bibtex-additional-search-fields '(keywords journal))

   (advice-add 'bibtex-completion-candidates
               :filter-return 'reverse)
)))

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

    (setq bibtex-completion-bibliography '("~/ownCloud/areas/research/latex/zotero.bib")
          bibtex-completion-library-path '("~/ownCloud/areas/research/publications/")
          bibtex-completion-notes-path "~/ownCloud/org/roam/"
          bibtex-completion-notes-template-multiple-files (format "#+TITLE: ${title}\n#+ROAM_KEY: cite:${=key=}\n\n")

      bibtex-completion-additional-search-fields '(keywords)
      bibtex-completion-pdf-open-function
          (lambda (fpath)
            (start-process "okular" "*okular*" "okular" fpath)))

    (setq reftex-default-bibliography '("~/ownCloud/areas/research/latex/zotero.bib"))
))

;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
