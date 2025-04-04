(require 'kv)

;;
;; I should cache certain kinds of errors:
;; I think mostly 404's, and only if its' on perminent?
;; Maybe for a limited time?
;;

(defconst m-url-cache-store-name "url-cache")

(defun m-url-cache-current-time-sec ()
  (time-to-seconds (current-time)))

(defun m-url-cache-entry-expired (entry)
  (ignore-errors
      (and (assoc-get :ttl-sec entry)
           (> (m-url-cache-current-time-sec) (+ (assoc1 :create-time entry)
                                                (assoc1 :ttl-sec entry))))))

(defun m-url-cache-set (url data &optional ttl-sec)
  (kv-set-pp m-url-cache-store-name
             url `((:data . ,(pp-to-string data))
                   (:ttl-sec . ,ttl-sec)
                   (:create-time . ,(m-url-cache-current-time-sec)))))

(defun m-url-cache-get (url)
  (when-let (entry (ignore-errors (kv-get-pp m-url-cache-store-name url)))
    ;; Is there an expiration?
    (if (not (m-url-cache-entry-expired entry))
        (read (assoc1 :data entry))
      (progn
        (m-url-cache-forget url)
        nil))))

(defun m-url-cache-forget (url)
  (kv-delete m-url-cache-store-name url)
  (message "m-url-cache-forget %s" url))

(defun m-url-cache-prefix (m-url-prefix)
  (cl-loop for url in (kv-keys m-url-cache-store-name)
           when (string-starts-with url m-url-prefix)
             do (m-url-cache-forget url)))

(cl-defun m-url-cache-or-fetch (url fetch-fn &key ttl-sec)
  (if-let (resp (m-url-cache-get url))
      resp
    (when-let (resp (funcall fetch-fn))
      (m-url-cache-set url resp ttl-sec)
      resp)))

(defun m-url-cache--find-entries-to-purge ()
  (let ((to-delete))
    (kv-list-pp-cb m-url-cache-store-name
                   (fn (url entry)
                     (when (m-url-cache-entry-expired entry)
                       (push url to-delete))))
    to-delete))

(defun m-url-cache--list-timer-functions ()
  (mapcar #'timer--function timer-list))

(defun m-url-cache-purge ()
  (message "Purge url cache.")
  (dolist (expire-url (m-url-cache--find-entries-to-purge))
    (m-url-cache-forget expire-url)))

(defun m-url-cache-init ()
  (when (not (store-exists-p m-url-cache-store-name))
    (kv-create m-url-cache-store-name))

  (let ((purge-fn #'m-url-cache-purge))
    (unless (member purge-fn (m-url-cache--list-timer-functions))
      (run-at-time "60min" (* 60 60) purge-fn))))

(provide 'm-url-cache)
