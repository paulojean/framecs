(require 'f)

(defvar framecs-support-path
  (f-dirname load-file-name))

(defvar framecs-features-path
  (f-parent framecs-support-path))

(defvar framecs-root-path
  (f-parent framecs-features-path))

(add-to-list 'load-path framecs-root-path)

;; Ensure that we don't load old byte-compiled versions
(let ((load-prefer-newer t))
  (require 'framecs)
  (require 'espuds)
  (require 'ert))

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
