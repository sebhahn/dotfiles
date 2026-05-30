;;; packages.el --- my-typst layer packages file for Spacemacs.
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

;;; Code:

(defconst my-typst-packages
  '(typst-ts-mode))

(defun my-typst/init-typst-ts-mode ()
  (use-package typst-ts-mode
    :mode "\\.typ\\'"
    :init
    (require 'treesit nil t)
    (add-to-list 'treesit-language-source-alist
                 '(typst "https://github.com/uben0/tree-sitter-typst"))
    :config
    (setq typst-ts-math-script-display '(nil . nil))
    (setq typst-ts-preview-function
          (lambda (pdf)
            (start-process "typst-preview" nil "xreader" (expand-file-name pdf))))
    (defun my-typst-toggle-script-display ()
      "Toggle raised/lowered rendering of math super/subscripts."
      (interactive)
      (setq typst-ts-math-script-display
            (if (car typst-ts-math-script-display)
                '(nil . nil)
              '((raise -0.5) . (raise 0.5))))
      (font-lock-flush))
    (spacemacs/declare-prefix-for-mode 'typst-ts-mode "mc" "compile")
    (spacemacs/set-leader-keys-for-major-mode 'typst-ts-mode
      "cc" #'typst-ts-compile
      "cp" #'typst-ts-compile-and-preview
      "w"  #'typst-ts-watch-mode
      "p"  #'typst-ts-preview
      "ts" #'my-typst-toggle-script-display)))
