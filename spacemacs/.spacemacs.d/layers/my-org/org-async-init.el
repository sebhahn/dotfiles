(require 'package)
(setq package-enable-at-startup nil)
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
   (dot . t)
   (ruby . t)
   (R . t)
   (gnuplot . t)
   (latex . t))
 )

(setq org-confirm-babel-evaluate nil)
