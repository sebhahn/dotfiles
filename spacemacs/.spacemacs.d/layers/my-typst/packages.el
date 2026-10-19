;;; packages.el --- my-typst layer packages file for Spacemacs.  -*- lexical-binding: t -*-
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
    ;; tinymist language server (lsp-mode ships the client in lsp-typst.el)
    (add-hook 'typst-ts-mode-local-vars-hook #'lsp-deferred)
    ;; Pin tinymist to the project's main file, so included files see the
    ;; bibliography and bindings of the root document. Set per project in
    ;; .dir-locals.el: ((typst-ts-mode . ((my-typst-main-file . "main.typ"))))
    ;; (projectResolution "lockDatabase" does not work: lsp-mode's config push
    ;; after startup makes tinymist drop it.)
    (defvar-local my-typst-main-file nil
      "Main Typst file, relative to the .dir-locals.el that sets it.")
    (put 'my-typst-main-file 'safe-local-variable #'stringp)
    (defun my-typst--main-file ()
      "Absolute path of `my-typst-main-file', or nil if unset."
      (when-let* ((main my-typst-main-file)
                  (dir (locate-dominating-file default-directory ".dir-locals.el")))
        (expand-file-name main dir)))
    (defun my-typst-pin-main ()
      "Pin tinymist to `my-typst-main-file'."
      (interactive)
      (when-let* ((main (my-typst--main-file)))
        (lsp-send-execute-command "tinymist.pinMain" (vector main))))
    (add-hook 'lsp-after-open-hook
              (lambda () (when (derived-mode-p 'typst-ts-mode) (my-typst-pin-main))))
    :config
    (setq typst-ts-math-script-display '(nil . nil))
    ;; sub/superscript faces ship at :height 0.85, shrinking the glyphs
    ;; independently of the raise above; keep them the same size as math text
    (set-face-attribute 'typst-ts-subscript-face nil :height 1.0)
    (set-face-attribute 'typst-ts-superscript-face nil :height 1.0)
    (setq typst-ts-preview-function
          (lambda (pdf)
            (when pdf
              (start-process "evince" nil "evince" pdf))))
    (defun my-typst-toggle-script-display ()
      "Toggle raised/lowered rendering of math super/subscripts."
      (interactive)
      (setq typst-ts-math-script-display
            (if (car typst-ts-math-script-display)
                '(nil . nil)
              '((raise -0.5) . (raise 0.5))))
      (font-lock-flush))
    (defun my-typst-compile-main ()
      "Compile `my-typst-main-file' instead of the current file."
      (interactive)
      (if-let* ((main (my-typst--main-file)))
          (with-current-buffer (find-file-noselect main)
            (typst-ts-compile))
        (user-error "`my-typst-main-file' is not set for this project")))
    (defun my-typst--target-buffer ()
      "Buffer of the main file if set, else the current buffer."
      (if-let* ((main (my-typst--main-file)))
          (find-file-noselect main)
        (current-buffer)))
    (defun my-typst-preview ()
      "Preview the main file's PDF, or the current file's if no main is set."
      (interactive)
      ;; run from the target buffer: the PDF path is relative to its directory
      (with-current-buffer (my-typst--target-buffer)
        (typst-ts-preview)))
    (defun my-typst-compile-and-preview ()
      "Compile and preview the main file, or the current file if no main is set."
      (interactive)
      (with-current-buffer (my-typst--target-buffer)
        (typst-ts-compile t)))
    (defun my-typst-watch ()
      "Toggle watching the main file, or the current file if no main is set."
      (interactive)
      (with-current-buffer (my-typst--target-buffer)
        (typst-ts-watch-mode 'toggle)))
    (spacemacs/declare-prefix-for-mode 'typst-ts-mode "mc" "compile")
    (spacemacs/declare-prefix-for-mode 'typst-ts-mode "ms" "search")
    (spacemacs/set-leader-keys-for-major-mode 'typst-ts-mode
      "cm" #'my-typst-compile-main
      "o"  #'typst-ts-mc-open-at-point
      "'"  #'typst-ts-edit-indirect
      "ss" #'typst-ts-mc-search-typst-symbol
      "sp" #'typst-ts-mc-search-package
      "P"  #'my-typst-pin-main
      "cc" #'typst-ts-compile
      "cp" #'my-typst-compile-and-preview
      "w"  #'my-typst-watch
      "p"  #'my-typst-preview
      "v"  #'my-typst-preview
      "ts" #'my-typst-toggle-script-display)))
