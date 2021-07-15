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
   '(
     ;; package research-configs go here
     ;; parsebib
     helm-bibtex
     ;; reftex
     hydra
     key-chord
     org-ref
     )
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
       )
     :config
     (progn

   (setq helm-bibtex-library-path "~/ownCloud/research/publications")
   (setq helm-bibtex-bibliography "~/ownCloud/research/latex/zotero.bib")
   (setq bibtex-completion-bibliography "~/ownCloud/research/latex/zotero.bib")
   (setq helm-bibtex-notes-path "~/ownCloud/org/roam")
   (setq bibtex-notes-path "~/ownCloud/org/roam")
   (setq bibtex-completion-notes-path "~/ownCloud/org/roam")

   (setq helm-bibtex-pdf-open-function
     (lambda (fpath)
       (start-process "okular" "*okular*" "okular" fpath)))

   (setq helm-bibtex-additional-search-fields '(keywords journal))

   (setq bibtex-completion-notes-template-multiple-files
         (format "#+TITLE: ${title}\n#+ROAM_KEY: cite:${=key=}\n\n"))

   (advice-add 'bibtex-completion-candidates
               :filter-return 'reverse)
)))

(defun research-config/post-init-org-ref()
  "Init org ref package"
  (use-package org-ref
    :defer t
    :config

    (require 'org-ref-bibtex)

    (setq reftex-default-bibliography '("/home/shahn/ownCloud/research/latex/zotero.bib"))

    ;; (setq org-ref-bibliography-notes "/home/shahn/ownCloud/org/roam/pub_note.org")
    (setq org-ref-default-bibliography '("/home/shahn/ownCloud/research/latex/zotero.bib"))
    (setq org-ref-pdf-directory "/home/shahn/ownCloud/research/publications")

    ;; Tell org-ref to let helm-bibtex find notes for it
    (setq org-ref-notes-function
          (lambda (thekey)
            (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
              (bibtex-completion-edit-notes
               (list (car (org-ref-get-bibtex-key-and-file thekey)))))))

    (defun org-ref-include-default-bibliography (backend)
      "Add bibliographystyle and bibliography links on export if they are needed."
      (cond
      ((eq backend 'latex)
        (let* ((links (org-element-map (org-element-parse-buffer) 'link #'identity))
        (cites (-filter (lambda (link)
              (member (org-element-property :type link) org-ref-cite-types))
            links))
        (style (-filter (lambda (link)
              (string= (org-element-property :type link) "bibliographystyle"))
            links))
        (bibliography (-filter (lambda (link)
                (string= (org-element-property :type link) "bibliography"))
              links)))
          (when cites
      (unless style
        (goto-char (point-max))
        (insert "\nbibliographystyle:unsrt"))
      (unless bibliography
        (goto-char (point-max))
        (insert (format
          "\nbibliography:%s"
          (mapconcat (lambda (x)
            (file-relative-name x (file-name-directory (buffer-file-name))))
                org-ref-default-bibliography ",")))))))))


    (add-hook 'org-export-before-processing-hook #'org-ref-include-default-bibliography)))

(defun research-config/post-init-hydra ())

(defun research-config/init-key-chord ()
  (use-package key-chord
    :defer t))

;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
