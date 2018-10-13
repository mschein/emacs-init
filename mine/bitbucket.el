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
(require 'elisp-lib)


(defun bitbucket-create (server-url &optional user)
  (let ((bb `((:url . ,server-url))))
    (when user
      (append! bb (cons :user user)))
    bb))

(defun bitbucket-get-password ()
  (read-passwd "Bitbucket password: "))

(defun bitbucket-request (bb path &rest args)
  (let* ((url (path-join (assoc1 :url bb) path))
         (auth (when-let (user (cdr (assoc :user bb)))
                 (concat user ":" (bitbucket-get-password))))
         (resp (apply #'web-request url :auth auth args)))
    (when auth
        (append! resp (cons :auth auth)))
    resp))

(defun bitbucket-next-page (bb resp)
  (when-let (start (cdr (assoc 'nextPageStart (assoc1 :json resp))))
    (message "Start is: %d" start)
    (web-request (assoc1 :url resp)
                 :auth (cdr (assoc :auth bb))
                 :op (assoc1 :op resp)
                 :params (cons
                          (cons "start" start)
                          (assoc1 :params resp)))))

(defun bitbucket-request-all (bb &rest args)
  (let ((values [])
        (resp (apply #'bitbucket-request bb args)))
    (while resp
      (setf values (vconcat values (assoc1 '(:json values) resp)))
      (setf resp (bitbucket-next-page bb resp)))
    values))

(defun bitbucket-list-projects (bb)
  (bitbucket-request-all bb "projects"))

(defun bitbucket-list-project-names (bb)
  (mapcar (| assoc1 'key %) (bitbucket-list-projects bb)))

(defun bitbucket-list-repos (bb project)
  (bitbucket-request-all bb (path-join "projects" project "repos")))

(defun bitbucket-list-repo-names (bb project)
  (mapcar (| assoc1 'name %) (bitbucket-list-repos bb project)))

(provide 'bitbucket)
