;;; -*- lexical-binding: t -*-
(require 'package)
(setq package-enable-at-startup nil)
;; Spacemacs keeps installed packages in elpa/<emacs-version>/develop/, which
;; plain `package-initialize' does not scan; point `package-user-dir' there
;; so babel libraries from installed packages (e.g. ob-mermaid) load here.
(setq package-user-dir
      (expand-file-name (format "elpa/%s/develop" emacs-version)
                        user-emacs-directory))
(package-initialize)

(require 'org)
(require 'ox)
(require 'ox-beamer)
(require 'ox-latex)
(require 'cl-lib)
(setq org-export-async-debug nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (ditaa . t)
   (plantuml . t)
   (mermaid . t)
   (dot . t)
   (ruby . t)
   (R . t)
   (gnuplot . t)
   (latex . t))
 )

(setq org-confirm-babel-evaluate nil)
