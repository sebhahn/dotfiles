(spacemacs/declare-prefix "of" "fabric")
(spacemacs/set-leader-keys
  "ofr" #'my-fabric/run-on-region
  "ofb" #'my-fabric/run-on-buffer)
