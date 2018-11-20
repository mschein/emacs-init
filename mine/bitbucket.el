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
(defconst bitbucket-password-key "bitbucket-pass")


(defun bitbucket-create (server-url &optional user)
  (let ((bb `((:url . ,(path-join server-url "rest/api" bitbucket-api-version)))))
    (when user
      (append-cons! bb :user user))
    bb))

(defun bitbucket-request-common (bb path &rest args)
  (apply #'web-request (path-join (assoc1 :url bb) path)
         :throw t args))

(defun bitbucket-make-auth (bb password)
  (concat (assoc1 :user bb) ":" password))

(defun bitbucket-verify-password (bb password)
  (bitbucket-request-common bb "projects"
                            :auth (bitbucket-make-auth bb password)))

(defun bitbucket-get-password (bb)
  ;; Cache this, so it's not insanely annoying.
  (if (password-in-cache-p bitbucket-password-key)
      (password-read-from-cache bitbucket-password-key)
    (progn
      (let ((pass (password-read "Bitbucket password: ")))
        (bitbucket-verify-password bb pass)
        (password-cache-add bitbucket-password-key pass)
        pass))))

(defun bitbucket-get-auth (bb)
  (when (assoc :user bb)
      (bitbucket-make-auth bb (bitbucket-get-password bb))))

(defun bitbucket-request (bb path &rest args)
  (assoc1 :json (apply #'bitbucket-request-common bb path
                       :auth (bitbucket-get-auth bb)
                       args)))

(defun bitbucket-inject-param (new-param in-args)
  (let* ((copied-args (copy-list in-args))
         (params (plist-get copied-args :params)))
         (plist-put copied-args :params (cons new-param params))))

(defun bitbucket-request-all (bb path &rest args)
  (let ((values [])
        (auth (bitbucket-get-auth bb))
        (resp nil))
    (cl-flet ((do-request (args)
                 (apply #'bitbucket-request-common bb path :auth auth args)))
      (setf resp (do-request args))
      (while resp
        (setf values (vconcat values (assoc1 '(:json values) resp)))
        (if-let (start (cdr (assoc 'nextPageStart (assoc1 :json resp))))
            (let ((args (bitbucket-inject-param (cons 'start start) args)))
              (setf resp (do-request args)))
          (setf resp nil))))
    values))

(defun bitbucket-application-properties (bb)
  (bitbucket-request bb "application-properties"))

(defun bitbucket-version (bb)
  (assoc1 '(:json version) (bitbucket-application-properties)))

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
                 (fromRef . ,(bitbucket-ref-dict branch project repo))
                 (toRef . ,(bitbucket-ref-dict "master" project repo)))))

    (when reviewers
      (append-atom! json `(reviewers . ,(vector (mapcar (fn (reviewer) `((user . ((name . ,reviewer))))) reviewers)))))

    (bitbucket-request bb (path-join "projects" project "repos" repo "pull-requests")
                       :op "POST"
                       :json json)))

(defun bitbucket-fetch-pull-requests (bb project repo)
  (bitbucket-request-all bb (path-join "projects" project "repos" repo "pull-requests")))

(defun bitbucket-fetch-pull-request (bb project repo id)
  (bitbucket-request bb (path-join "projects" project "repos" repo "pull-requests"
                                   (format "%s" id))))

(defun bitbucket-fetch-pull-request-activites (bb project repo id)
  (bitbucket-request-all bb (path-join "projects" project "repos" repo "pull-requests" (format "%s" id) "activities")))

(defun bitbucket-fetch-pull-request-comments (bb project repo id)
  (cl-loop for activity across (bitbucket-fetch-pull-request-activites bb project repo id)
           for comment = (cdr (assoc 'comment activity))
           if (and comment (> (length comment) 0))
           collect comment))

(defun bitbucket-inbox (bb &optional role)
  (bitbucket-request-all bb "inbox/pull-requests"
                         :params (when role
                                   `((role . ,role)))))

(provide 'bitbucket)
