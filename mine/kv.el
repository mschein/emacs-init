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

(defun kv-create (store-name)
  (store-create-store store-name)

  ;; XXX Turn this into the schema
  (with-store store-name
    (sqlite3-exec db (format "CREATE TABLE %s (key TEXT PRIMARY KEY, value TEXT);"
                             kv--table-name))))

(defun kv-set (store-name key value)
  (with-store store-name
    (with-mysqlite3-stmt db "INSERT INTO kv VALUES (?, ?)"
      (sqlite3-bind-multi stmt key value)
      (sqlite3-step stmt)))
  key)

(defun kv-add-key-values (store-name kvs)
  (with-store store-name
    (with-mysqlite3-stmt db "INSERT INTO kv VALUES (?, ?)"
      (cl-loop for (k . v) in kvs do
               (assert (stringp k))
               (sqlite3-bind-multi stmt k v)
               (sqlite3-step stmt)
               (sqlite3-reset stmt)))))

(defun kv-set-json (store-name key data)
  (kv-set store-name key (json-encode data)))

(defun kv-get (store-name key)
  (let ((result))
    (with-store store-name
      (with-mysqlite3-stmt db "SELECT value FROM kv WHERE key = ?;"
        (sqlite3-bind-multi stmt key)
        (let ((res (sqlite3-step stmt)))
          (if (= sqlite-row res)
              (setf result (first (sqlite3-fetch stmt)))
            (error "Unable to retrieve `%s`: %s" key res)))))
    result))

(defun kv-get-json (store-name key)
  (json-read-from-string (kv-get store-name key)))

(defun kv-delete (store-name key)
  (with-store store-name
      (with-mysqlite3-stmt db "DELETE FROM kv WHERE key = ?;"
        (sqlite3-bind-multi stmt key)
        (sqlite3-step stmt))))

(defun kv--select-statement (store-name statement)
  (let ((output '()))
    (with-store store-name
      (with-mysqlite3-stmt db statement
        (while (= sqlite-row (sqlite3-step stmt))
          (push (sqlite3-fetch stmt) output))))

    (nreverse output)))

(defun kv-list (store-name)
  (mapcar (| cons (first %) (second %))
          (kv--select-statement store-name "SELECT * from kv;")))

(defun kv-list-json (store-name)
  (mapcar (| cons (car %) (json-read-from-string (cdr %)))
          (kv-list store-name)))

(defun kv-list-keys (store-name)
  (mapcar #'first (kv--select-statement store-name "SELECT key from kv;")))

(defun kv-list-values (store-name)
  (mapcar #'first (kv--select-statement store-name "SELECT value from kv;")))

(defun kv-list-values-json (store-name)
  (mapcar #'json-read-from-string (kv-list-values store-name)))

(provide 'kv)