;;; -*- lexical-binding: t -*-
;;;
;;; Code for database access to the s3-cache store.
;;;
(require 'store)

;; Use this to access the database
(defconst s3-cache-store "s3-cache" "Constant to use with `with-store' to access the database.")

;; to start do: (store-create-store "s3-cache" :use-metadata t)

;; Database functions

(defun s3-cache--current-time ()
  (time-to-seconds (current-time)))

(defun s3-cache--get-row-metadata (db bucket key)
  (sqlite-select db "SELECT etag, last_ping FROM s3_cache WHERE bucket = ? AND key = ?;" (list bucket key)))

(defun s3-cache--get-row-data (db bucket key)
  (sqlite-select db "SELECT data FROM s3_cache WHERE bucket = ? AND key = ?;" (list bucket key))

(defun s3-cache--drop-row (db bucket key)
  (sqlite-execute db "DELETE FROM s3_cache WHERE bucket = ? AND key = ?;" (list bucket key)))

(cl-defun s3-cache--insert-row (db &key bucket key etag data)
  (sqlite-execute db "INSERT INTO s3_cache(bucket, key, etag, data, last_ping)
                      VALUES (?, ?, ?, ?, ?);" (list bucket key etag data (s3-cache--current-time))))

(cl-defun s3-cache--update-row-ping (db bucket key)
  (sqlite-execute db "UPDATE s3_cache
                      SET last_ping = ?
                      WHERE bucket = ? AND key = ?;"
                  (list (s3-cache--current-time) bucket key)))

(cl-defun s3-cache-get-obj-json (bucket key fetch-fn &key ping-timeout-sec)
  (let ((db-etag nil)
        (last-ping-sec nil)
        (current-time-sec (s3-cache--current-time))
        (do-fetch-data nil))

    (message "Check the s3 cache for %s %s" bucket key)
    (with-store-txn s3-cache-store
      ;; Can i use names in the sql?
      ;; Get metadata from the database
      (when-let (res (s3-cache--get-row-metadata db bucket key))
        (message "Cache entry exists: %s %s"  db-etag last-ping-sec)

        (setf db-etag (first res))
        (setf last-ping-sec (second res)))

      ;; Do we need to check the etag?
      (when (or (not last-ping-sec)
                (and ping-timeout-sec
                     (> (- current-time-sec last-ping-sec) ping-timeout-sec)))
        (message "Time to refetch the data")
        (setf do-fetch-data t))

      (json-read-from-string
       (if-let (response (and do-fetch-data
                              (funcall fetch-fn db-etag)))
           (progn
             (message "Got the full data response from s3, so save it.")
             (s3-cache--drop-row db bucket key)
             (s3-cache--insert-row db :bucket bucket :key key
                                   :etag (assoc1 :etag response)
                                   :data (assoc1 :data response))
             (message "Rows saved.  data len: %s" (length (assoc1 :data response)))
             (assoc1 :data response))

         (progn
           (message "Use the cached data: etag: %s" db-etag)
           (let ((data (s3-cache--get-row-data db bucket key)))
             (message "Update the rows metadata")
             (s3-cache--update-row-ping db bucket key)
             data))))))))

(provide 's3-cache)


