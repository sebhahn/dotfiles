(spacemacs/declare-prefix "of" "fabric")
(spacemacs/set-leader-keys
  "ofr" #'my-fabric/run-on-region
  "ofb" #'my-fabric/run-on-buffer
  "off" #'my-fabric/run-on-file
  "ofm" #'my-fabric/select-model
  "ofk" #'my-fabric/kill)
