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

(defun kv-create (store-name)
  (store-create-store store-name
                      :schema (format "CREATE TABLE %s (key TEXT PRIMARY KEY, value TEXT);"
                                      kv--table-name)))

(defconst kv-upsert-string "INSERT INTO kv(key, value) VALUES (?, ?)
                            ON CONFLICT (key) DO UPDATE SET value=excluded.value")

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

(defun kv-check (store-name key)
  (ignore-errors (kv-get store-name key)))

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
  (with-store store-name
      (sqlite-execute db "DELETE FROM kv WHERE key = ?;" (list key))))


;;
;; TODO: switch to using sets for this, to handle
;; large data sets better.
;; 
(defun kv--select-statement-single-value (store-name statement)
  (with-store store-name
    (mapcar #'first (sqlite-select db statement))))

(defun kv-list (store-name)
  (with-store store-name
    (mapcar (fn ((key value))
              (cons key value))
            (sqlite-select db "SELECT * from kv;"))))

(defun kv-list-cb (store-name cb)
  (dolist (row (kv-list store-name))
    (funcall cb row)))

(defun kv-list-json (store-name)
  (mapcar (fn ((key . value))
            (cons key (when value
                        (json-read-from-string value))))
          (kv-list store-name)))

(defun kv-list-keys (store-name)
  (kv--select-statement-single-value store-name "SELECT key from kv;"))

(defun kv-list-values (store-name)
  (kv--select-statement-single-value store-name "SELECT value from kv;"))

(defun kv-list-values-json (store-name)
  (mapcar #'json-read-from-string (kv-list-values store-name)))

(provide 'kv)
