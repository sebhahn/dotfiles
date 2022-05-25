(spacemacs/set-leader-keys-for-major-mode 'python-mode
  "cn" 'my-python/python-execute-file)

(spacemacs/set-leader-keys-for-major-mode 'python-mode
  "cs" 'send-region-compilation
  "cS" 'send-region-compilation-dbg)
