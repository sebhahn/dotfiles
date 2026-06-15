(defconst my-fabric-patterns-directory
  (expand-file-name "fabric/patterns"
                     (or (getenv "XDG_CONFIG_HOME") "~/.config")))

(defun my-fabric--pattern-list ()
  "Return the list of available fabric pattern names."
  (when (file-directory-p my-fabric-patterns-directory)
    (directory-files my-fabric-patterns-directory nil "^[^.]")))

(defun my-fabric--read-pattern ()
  "Prompt for a fabric pattern name with completion."
  (completing-read "Fabric pattern: " (my-fabric--pattern-list) nil t))

(defun my-fabric--run (start end pattern)
  "Run fabric PATTERN on the text between START and END.
Shows the result in the *fabric-output* buffer."
  (let ((output-buffer (get-buffer-create "*fabric-output*")))
    (with-current-buffer output-buffer
      (erase-buffer)
      (when (fboundp 'markdown-mode)
        (markdown-mode)))
    (call-process-region start end "fabric" nil output-buffer nil "-p" pattern)
    (display-buffer output-buffer)))

(defun my-fabric/run-on-region (start end pattern)
  "Run a fabric PATTERN on the region from START to END."
  (interactive (list (region-beginning) (region-end) (my-fabric--read-pattern)))
  (my-fabric--run start end pattern))

(defun my-fabric/run-on-buffer (pattern)
  "Run a fabric PATTERN on the whole buffer."
  (interactive (list (my-fabric--read-pattern)))
  (my-fabric--run (point-min) (point-max) pattern))
