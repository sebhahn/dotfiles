;;; packages.el --- my-chatgpt layer packages file for Spacemacs.
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
;; added to `my-chatgpt-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-chatgpt/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-chatgpt/pre-init-PACKAGE' and/or
;;   `my-chatgpt/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-chatgpt-packages
  '(chatgpt-shell))

(defun my-chatgpt/init-chatgpt-shell ()
  (use-package chatgpt-shell
    :ensure t
    :custom
    ((chatgpt-shell-openai-key (getenv "AQUEDUCT_API_KEY"))
     (chatgpt-shell-api-url-base "https://aqueduct.ai.datalab.tuwien.ac.at")))

  (with-eval-after-load 'chatgpt-shell
    (defvar my-chatgpt-shell-extra-openai-models
      (list
       (chatgpt-shell-openai-make-model
        :version "glm-4.5-106b"
        :function-calling t
        :token-width 3
        :context-window 400000)
       (chatgpt-shell-openai-make-model
        :version "e5-mistral-7b"
        :function-calling t
        :token-width 3
        :context-window 400000)
       (chatgpt-shell-openai-make-model
        :version "glm-4.6-355b"
        :function-calling t
        :token-width 3
        :context-window 400000))
      "Extra models to append to `chatgpt-shell-openai-models'.")

    (defun my-chatgpt-shell--append-custom-models (models)
      (nconc (copy-sequence models) my-chatgpt-shell-extra-openai-models))

    (advice-add 'chatgpt-shell-openai-models :filter-return
                #'my-chatgpt-shell--append-custom-models))

  ;; Flag to ensure we only reload once
  (defvar my-chatgpt-shell--models-loaded nil
    "Whether ChatGPT models have been reloaded once after startup.")

  (defun my-chatgpt-shell--reload-once ()
    "Reload default and custom models the first time a ChatGPT shell is opened."
    (unless my-chatgpt-shell--models-loaded
      (setq my-chatgpt-shell--models-loaded t)
      (chatgpt-shell-reload-default-models)
      ;; (setq chatgpt-shell-model "glm-4.5-355b")
      (setq chatgpt-shell-model-version "glm-4.5-106b")
      (setq chatgpt-shell-model "glm-4.5-106b")
      (message "[my-chatgpt] Reloaded ChatGPT models (including custom ones).")))

  ;; Hook into shell open
  (add-hook 'chatgpt-shell-mode-hook #'my-chatgpt-shell--reload-once))
