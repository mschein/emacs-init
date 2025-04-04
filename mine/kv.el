;;; -*- lexical-binding: t -*-
;;;
;;; Code for a simple key value store.
;;;
;;; This provides a simple way to store information in the
;;; file system.  It's based on sqlite3 and my store library.
;;;
;;; It's also meant as a reference implementation for things
;;; you can build on top of store.
;;;

(require 'store)

(defconst kv--table-name "kv")

;; Is there a need for any metadata about the stores?
;;
;; Note that you should use the "store-*" functions
;; to check for existence  and remove any kv-store
;;
;; I wonder if a "with-kv" function would be useful
;; or just a kv dynamic variable.
;;
;; new ideas for kv:
;; 1. create and updated fields, to allow proper function of ybl
;; 2. type? So you can convert each field?
;; 3. Metadata table?
;;    name, creation date, data type?
;;

(defun kv-create (store-name)
  (store-create-store
   store-name
   :schema
   (format "CREATE TABLE %s (key TEXT PRIMARY KEY,
            value TEXT,
            create_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            update_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP);

            CREATE TRIGGER kv_update_trigger AFTER UPDATE ON %s
            BEGIN
                UPDATE kv SET update_time = CURRENT_TIME WHERE key = new.key;
            END;"
           kv--table-name kv--table-name)))

(defconst kv-upsert-string "INSERT INTO kv(key, value) VALUES (?, ?)
                            ON CONFLICT (key) DO UPDATE SET
                            value=excluded.value")

(defun kv-set (store-name key value)
  (with-store store-name
    (sqlite-execute db kv-upsert-string (list key value))
    key))

(defun kv-add-key-values (store-name kvs)
  (with-store store-name
    (dolist (kv kvs)
      (sqlite-execute db kv-upsert-string kv))))

(defun kv-set-json (store-name key data)
  (kv-set store-name key (json-encode data)))

(defun kv-set-pp (store-name key data)
  (kv-set store-name key (pp-to-string data)))

(defun kv-get (store-name key)
  (with-store store-name
    (caar (sqlite-select db "SELECT value FROM kv WHERE key = ?;" (list key)))))

(defun kv-exists-p (store-name key)
  (not (not
        (ignore-errors
          (with-store store-name
            (sqlite-select db "SELECT key from kv WHERE key = ?;" (list key)))))))

(defun kv-get-json (store-name key)
  (json-read-from-string (kv-get store-name key)))

(defun kv-check-json (store-name key)
  (ignore-errors (kv-get-json store-name key)))

(defun kv-get-pp (store-name key)
  (when-let (data (kv-get store-name key))
    (read data)))

(defun kv-check-pp (store-name key)
  (ignore-errors (kv-get-pp store-name key)))

(defun kv-delete (store-name key)
  ;; TODO(mls): add error checking.
  (with-store-txn store-name
    (sqlite-execute db "DELETE FROM kv WHERE key = ?;" (list key))))

;;
;; TODO: switch to using sets for this, to handle
;; large data sets better.
;;
(defun kv--select-statement-single-value (store-name statement)
  (with-store store-name
    (mapcar #'first (sqlite-select db statement))))

(defun kv-list-core-cb (store-name keys cb)
  (with-store-select-set (store-name (format "SELECT %s from kv;"
                                             (string-join keys ", ")))
      (cl-loop while (sqlite-more-p stmt) do
               (let ((data (sqlite-next stmt)))
                 (when data
                   (apply cb data))))))

(cl-defun kv-list-cb (store-name cb)
  (kv-list-core-cb store-name (list "key" "value") cb))

(cl-defun kv-list (store-name)
  (let ((output))
    (kv-list-cb store-name
                (fn (key value)
                  (pushcons key value output)))
    (nreverse output)))

;; Don't add offset a limit.  they don't make sense for a kv store
(cl-defun kv-list-json-cb (store-name cb)
  (kv-list-cb store-name
              (fn (key value)
                (funcall cb key (when value
                                  (json-read-from-string value))))))

(cl-defun kv-list-json (store-name)
  (let ((output))
    (kv-list-json-cb store-name
                     (fn (key value)
                       (pushcons key value output)))
    output))


(defun kv-list-pp-cb (store-name cb)
  (kv-list-cb store-name
              (fn (key value)
                (funcall cb key (when value
                                  (read value))))))
(cl-defun kv-list-pp (store-name)
  (let ((output))
    (kv-list-pp-cb store-name
                   (fn (key value)
                     (pushcons key value output)))
    output))

(defun kv-keys (store-name)
  (kv--select-statement-single-value store-name "SELECT key from kv;"))

(defun kv-keys-cb (store-name cb)
  (kv-list-core-cb store-name (list "key") cb))

(defun kv-values (store-name)
  (kv--select-statement-single-value store-name "SELECT value from kv;"))

(defun kv-values-cb (store-name cb)
  (kv-list-core-cb store-name (list "value") cb))

(defun kv-values-json (store-name)
  (mapcar #'json-read-from-string (kv-values store-name)))

(provide 'kv)
