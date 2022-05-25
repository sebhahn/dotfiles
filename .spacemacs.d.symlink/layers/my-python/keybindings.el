(spacemacs/set-leader-keys-for-major-mode 'python-mode
  "cn" 'my-python/python-execute-file)

(spacemacs/set-leader-keys-for-major-mode 'python-mode
  "cs" 'my-python/send-region-compilation
  "cS" 'my-python/send-region-compilation-dbg)
