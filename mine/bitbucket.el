;;;
;;; Code for communicating with a bitbucket server.
;;;
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl-lib)
(require 's)
(require 'dash)


(defstruct bitbucket ()
  )

(defun bitbucket-open (server username)
  )

(defun bitbucket-get-password ()
  (read-passwd "Bitbucket password> "))

(defun bitbucket-get-pull-requests ())


(provide 'bitbucket)
