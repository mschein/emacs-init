(require 'kv)

(defconst url-cache-store-name "url-cache")

(defun url-cache-init ()
  (when (not (store-exists-p url-cache-store-name))
    (kv-create url-cache-store-name)))

(defun url-cache-current-time-sec ()
  (time-to-seconds (current-time)))

(defun url-cache-entry-expired (entry)
  (and (assoc-get 'ttl-sec entry)
       (> (url-cache-current-time-sec) (+ (assoc1 'create-time entry)
                                          (assoc1 'ttl-sec entry)))))

(defun url-cache-set (url data &optional ttl-sec)
  (kv-set-json url-cache-store-name
               url `((data . ,data)
                     (ttl-sec . ,ttl-sec)
                     (create-time . ,(url-cache-current-time-sec)))))

(defun url-cache-get (url)
  (when-let (entry (ignore-errors (kv-get-json url-cache-store-name
                                               url)))
    ;; Is there an expiration?
    (if (not (url-cache-entry-expired entry))
        (assoc1 'data entry)
      (progn
        (url-cache-forget url)
        nil))))

(defun url-cache-forget (url)
  (kv-delete url-cache-store-name url))

(defun url-cache-prefix (url-prefix)
  (cl-loop for url in (kv-list-keys url-cache-store-name)
           when (string-starts-with url url-prefix)
             do (url-cache-forget url)))

(provide 'url-cache)
