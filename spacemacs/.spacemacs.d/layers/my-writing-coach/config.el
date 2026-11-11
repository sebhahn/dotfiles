;;; config.el --- my-writing-coach layer config file for Spacemacs. -*- lexical-binding: t; -*-

;;; Code:

;; Runs after file- and dir-local variables are set, so `writing-coach: nil'
;; in a file or .dir-locals.el keeps the mode off.
(add-hook 'hack-local-variables-hook #'writing-coach-maybe-enable)

;;; config.el ends here
