;;; -*- lexical-binding: t -*-
;;;
;;; Copyright (c) by Michael Scheinholtz, All Rights Reserved.
;;;

;;;
;;; Code for talking to a Jenkins server.
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'cl-lib)
(require 'elisp-lib)


;;
;; server/api/
;;

(defun jenkins-create (server-url &optional user)
  (let ((jenkins-server-info `((:url . ,server-url))))
    (when user
      (append-cons! jenkins-server-info :user user))
    jenkins-server-info))

;; TODO(mls): share this better with bitbucket.
(defun jenkins-make-auth (jsi password)
  (concat (assoc1 :user jsi) ":" password))

(defun jenkins-verify-password (jsi password)
  ;; Try to get the get the root info
  (web-request-is-success (path-join (assoc1 :url jsi) "api" "json")
                          :auth (jenkins-make-auth jsi password)))

(defun jenkins-get-password (jsi)
  (read-user-password "Jenkins password: " "jenkins-password" (lambda (password) (jenkins-verify-password jsi password))))

(defun jenkins-get-auth (jsi password-fn)
  (when-let (user (assoc-get :user jsi))
    (jenkins-make-auth jsi (jenkins-get-password jsi))))

(cl-defun jenkins-request (jsi path &key params)
  (append-cons! params "pretty" "true")

  (assoc1 :json (web-request
                 (path-join (assoc1 :url jsi) path "api" "json")
                 :throw t
                 :auth (jenkins-get-auth jsi #'jenkins-get-password)
                 :params params)))

(defun jenkins-core-info (jsi)
  (jenkins-request jsi ""))


(defun jenkins-fetch-queue (jsi)
  (jenkins-request jsi "queue"))

(defun jenkins-overall-load (jsi)
  (jenkins-request jsi "overallLoad"))

(defun jenkins-list-jobs (jsi)
  (jenkins-request jsi "" :params '(("tree" . "jobs[name,color]"))))

(defun jenkins-list-nodes (jsi)
  (jenkins-request jsi "computer")
  )

(defun jenkins-fetch-job (jsi job-path)
  (jenkins-request jsi (path-join "job" job-path)))

(provide 'jenkins)
