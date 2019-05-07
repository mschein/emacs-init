;;;
;;; Code for communicating with a bitbucket server.
;;;

;; See: https://docs.atlassian.com/bitbucket-server/rest/6.0.0/bitbucket-rest.html?utm_source=%2Fstatic%2Frest%2Fbitbucket-server%2Flatest%2Fbitbucket-rest.html&utm_medium=301#idp151

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
  (let ((bb `((:server-root . ,server-url)
              (:url . ,(path-join server-url "rest/api" bitbucket-api-version)))))
    (when user
      (append-cons! bb :user user))
    bb))

(defun bitbucket-request-common (bb path &rest args)
  (apply #'web-request (path-join (assoc1 :url bb) path)
         :throw t args))

(defun bitbucket-make-auth (bb password)
  (concat (assoc1 :user bb) ":" password))

(defun bitbucket-verify-password (bb password)
  (condition-case nil
      (bitbucket-request-common bb "projects"
                                :auth (bitbucket-make-auth bb password))
    (error 'nil)))

(defun bitbucket-get-password (bb)
  (read-user-password "Bitbucket password: " bitbucket-password-key
                      (lambda (password) (bitbucket-verify-password bb password))))

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
  (assoc1 'version (bitbucket-application-properties bb)))

(defun bitbucket-list-projects (bb)
  (bitbucket-request-all bb "projects"))

(defun bitbucket-list-project-names (bb)
  (mapcar (| assoc1 'key %) (bitbucket-list-projects bb)))

(defun bitbucket-list-repos (bb project)
  (bitbucket-request-all bb (path-join "projects" project "repos")))

(defun bitbucket-list-repo-names (bb project)
  (mapcar (| assoc1 'name %) (bitbucket-list-repos bb project)))

(defun bitbucket-get-project (bb project)
  (bitbucket-request bb (path-join "projects" project)))

(defun bitbucket-get-repo (bb project repo)
  (bitbucket-request bb (path-join "projects" project "repos" repo)))

(defun bitbucket-update-repo (bb project repo options)
  (bitbucket-request bb (path-join "projects" project "repos" repo)
                     :method "POST"
                     :json options))

(defun bitbucket-get-repo-git-origin-url (bb project repo)
  (assoc1-traverse '(0 href)
                   (filter (fn (link)
                             (equal (assoc1 'name link) "ssh"))
                           (assoc1-traverse '(links clone) (bitbucket-get-repo bb project repo)))))

;; This doesn't seem to work correctly
;;
;; (bitbucket-query-repos bb-info-auth :repo "deployment-manager" :project "OE")
;; returns an empty set, even on the server rest api.
;;
;;
(cl-defun bitbucket-query-repos (bb &key repo project permission visibility)
  (when permission
    (assert (member permission '(REPO_READ REPO_WRITE REPO_ADMIN))))

  (when visibility
    (assert (member visibility '(public private))))

  (bitbucket-request-all bb "repos" :params (filter #'cdr
                                                    `(("name" . ,repo)
                                                      ("projectname" . ,project)
                                                      ("permission" . ,permission)
                                                      ("visibility" . ,visibility)))))

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
                       :method "POST"
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

(defun bitbucket-fetch-pull-request-diff (bb project repo id)
  (bitbucket-request bb (path-join "projects" project "repos" repo "pull-requests" (format "%s" id) "diff")))

(defun bitbucket-fetch-pull-request-diff-score (bb project repo id)
    (cl-loop for file across (assoc1 'diffs (bitbucket-fetch-pull-request-diff bb project repo id))
             for hunks = (assoc1 'hunks file)
             sum (cl-loop for hunk across hunks
                          sum (+ (assoc1 'sourceSpan hunk)
                                 (assoc1 'destinationSpan hunk)))))

(defun bitbucket-fecth-all-commits (bb project repo)
  (assoc1 'values (bitbucket-request-all bb (path-join "projects" project "repos" repo "commits"))))

(defun bitbucket-fetch-commits (bb project repo)
  ;; should i process the return values at all?
  (assoc1 'values (bitbucket-request bb (path-join "projects" project "repos" repo "commits"))))

(defun bitbucket-commit-url (bb project repo sha)
  (path-join (assoc1 :server-root bb) "projects" project "repos" repo "commits" sha))

(defun bitbucket-fetch-commit (bb project repo sha)
  (bitbucket-request bb (path-join "projects" project "repos" repo "commits" sha)))

(defun bitbucket-fetch-files (bb project repo)
  (bitbucket-request-all bb (path-join "projects" project "repos" repo "files")))

(defun bitbucket-repo-to-project (bb repo)
  ;;
  ;; search for a repo across all projects.
  ;;
  (let ((+webrequest-cache-urls+ (* 60 60 24 1)))
    (let ((result
           (cl-loop for project in (bitbucket-list-project-names bb) do
                    (condition-case nil
                        (progn
                          (message "Checking %s for repo %s" project repo)
                          (bitbucket-request bb (path-join "projects" project "repos" repo))
                          (message "Found project: %s" project)
                          (return project))
                      (error 'nil)))))
      (if result
          result
        (error "Unable to find a project for repo: %s" repo)))))

(defun bitbucket-repo-to-url-raw (bb repo &optional rest-api)
  (path-join (assoc1 (if rest-api
                         :url
                       :server-root) bb)
             "projects"
             (bitbucket-repo-to-project bb repo)
             "repos"
             repo))

(defun bitbucket-repo-to-url (bb repo &optional rest-api)
   (m-url-cache-or-fetch (string-join (list (assoc1 :url bb) repo rest-api) ":")
                         (lambda () (bitbucket-repo-to-url-raw bb repo))
                         :ttl-sec (* 60 60 24 5)))

(cl-defun bitbucket-inbox (bb &optional role)
  (bitbucket-request-all bb "inbox/pull-requests"
                         :params (remove-if (| not (cdr %))
                                            `((role . ,role)))))

(defun bitbucket-create-repo (bb project repo)
  (bitbucket-request bb (path-join "projects" project "repos")
                       :method "POST"
                       :json `((name . ,repo)
                               (forkable . "true"))))

(provide 'bitbucket)
