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

(defconst bitbucket-api-version "1.0" "The version of the bitbucket api we expect.")

(defun bitbucket-create (server-url &optional user)
  (let ((bb `((:url . ,(path-join server-url "rest/api" bitbucket-api-version)))))
    (when user
      (append! bb (cons :user user)))
    bb))

(defun bitbucket-get-password ()
  (read-passwd "Bitbucket password: "))

(defun bitbucket-request (bb path &rest args)
  (let* ((url (path-join (assoc1 :url bb) path))
         (auth (when-let (user (cdr (assoc :user bb)))
                 (concat user ":" (bitbucket-get-password))))
         (resp (apply #'web-request url :auth auth :throw t args)))
    (when auth
        (append! resp (cons :auth auth)))
    resp))

(defun bitbucket-next-page (bb resp)
  (when-let (start (cdr (assoc 'nextPageStart (assoc1 :json resp))))
    (web-request (assoc1 :url resp)
                 :auth (cdr (assoc :auth bb))
                 :op (assoc1 :op resp)
                 :params (cons
                          (cons "start" start)
                          (assoc1 :params resp)))))

(defun bitbucket-application-properties (bb)
  (bitbucket-request bb "application-properties"))

(defun bitbucket-version (bb)
  (assoc1 '(:json version) (bitbucket-application-properties)))

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

(defun bitbucket-repo-data-get-clone-url (bb-repo-data type)
  (cl-loop for clone-info across (assoc1 '(links clone) bb-repo-data)
           if (equal type (assoc1 'name clone-info))
              return (assoc1 'href clone-info)))

(defun bitbucket-repo-list-branches (bb project repo)
  (bitbucket-request bb (path-join "projects" project "repos" repo "branches")))

(defun bitbucket-ref-dict (branch project repo)
  `((id . ,(path-join "refs/heads" branch))
    (repository . ((slug . ,repo)
                   (project . ((key . ,project)))))))

(cl-defun bitbucket-open-pull-request (bb project repo &key title branch reviewers)
  (let* ((json `((title . ,title)
                 (fromRef . ,(bitbucket-ref-dict branch (concat "~" (assoc1 :user bb)) repo))
                 (toRef . ,(bitbucket-ref-dict "master" project repo)))))

    (when reviewers
      (append! json `(reviewers . ,(vector (mapcar (fn (reviewer) `((user . ((name . ,reviewer))))) reviewers)))))

    (bitbucket-request bb (path-join "projects" project "repos" repo "pull-requests")
                       :op "POST"
                       :json json)))

(provide 'bitbucket)
