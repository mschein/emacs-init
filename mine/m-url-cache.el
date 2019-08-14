(require 'kv)

;;
;; I should cache certain kinds of errors:
;; I think mostly 404's, and only if its' on perminent?
;; Maybe for a limited time?
;;

(defconst m-url-cache-store-name "url-cache")

(defun m-url-cache-init ()
  (when (not (store-exists-p m-url-cache-store-name))
    (kv-create m-url-cache-store-name)))

(defun m-url-cache-current-time-sec ()
  (time-to-seconds (current-time)))

(defun m-url-cache-entry-expired (entry)
  (and (assoc-get :ttl-sec entry)
       (> (m-url-cache-current-time-sec) (+ (assoc1 :create-time entry)
                                          (assoc1 :ttl-sec entry)))))

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
  (kv-delete m-url-cache-store-name url))

(defun m-url-cache-prefix (m-url-prefix)
  (cl-loop for url in (kv-list-keys m-url-cache-store-name)
           when (string-starts-with url m-url-prefix)
             do (m-url-cache-forget url)))

(cl-defun m-url-cache-or-fetch (url fetch-fn &key ttl-sec)
  (if-let (resp (m-url-cache-get url))
      resp
    (when-let (resp (funcall fetch-fn))
      (m-url-cache-set url resp ttl-sec)
      resp)))

(provide 'm-url-cache)
