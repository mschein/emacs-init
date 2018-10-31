;;;
;;; Code for a local store.
;;;

(require 'cl-lib)
(require 'elisp-lib)

;; How can I tentatively import this?
;; https://github.com/pekingduck/emacs-sqlite3-api
(require 'mysqlite3)

;;
;; Can I use sqlite for this?
;;

(defconst store--directory (expand-file-name "~/.emacs-data-store"))
(defconst store--kv-table "kv")


(defun store-get-path (store-name)
  (path-join store--directory (concat store-name ".db")))

(defun store-create-store (store-name &optional schema)
  (ensure-makedirs store--directory)

  (message "Create store at %s" (store-get-path store-name))
  (with-mysqlite3 ((store-get-path store-name))
    (sqlite3-exec db (format "CREATE TABLE %s (key TEXT PRIMARY KEY, value TEXT);"
                             store--kv-table))

  ;; XXX Add support for schemas
  ))

(defun store-exists-p (store-name)
  (ensure-makedirs store--directory)

  (file-exists-p (store-get-path store-name)))

(defun store-delete-store (store-name)
  (ensure-makedirs store--directory)
  (delete-file (store-get-path store-name)))

(defun store-set (store-name key value)
  (with-mysqlite3 ((store-get-path store-name))
    (with-mysqlite3-stmt db "INSERT INTO kv VALUES (?, ?)"
      (sqlite3-bind-multi stmt key value)
      (sqlite3-step stmt))))

(defun store-add-key-values (store-name kvs)
  (with-mysqlite3 ((store-get-path store-name))
    (with-mysqlite3-stmt db "INSERT INTO kv VALUES (?, ?)"
      (cl-loop for (k . v) in kvs do
               (sqlite3-bind-multi stmt k v)
               (sqlite3-step stmt)
               (sqlite3-reset stmt)))))

(defun store-set-json (store-name key data)
  (store-set store-name key (json-encode data)))

(defun store-get (store-name key)
  (let ((result))
    (with-mysqlite3 ((store-get-path store-name) :readonly)
      (with-mysqlite3-stmt db "SELECT value FROM kv WHERE key = ?;"
        (sqlite3-bind-multi stmt key)
        (let ((res (sqlite3-step stmt)))
          (if (= sqlite-row res)
              (setf result (first (sqlite3-fetch stmt)))
            (error "Unable to retrieve `%s`: %s" key res)))))
    result))

(defun store-get-json (store-name key)
  (json-read-from-string (store-get store-name key)))

(defun store-delete (store-name key)
  (with-mysqlite3 ((store-get-path store-name))
      (with-mysqlite3-stmt db "DELETE FROM kv WHERE key = ?;"
        (sqlite3-bind-multi stmt key)
        (sqlite3-step stmt))))

(defun store--select-statement (store-name statement)
  (let ((output '()))
    (with-mysqlite3 ((store-get-path store-name))
      (with-mysqlite3-stmt db statement
        (while (= sqlite-row (sqlite3-step stmt))
          (push (sqlite3-fetch stmt) output))))

    (nreverse output)))

(defun store-list (store-name)
  (mapcar (| cons (first %) (second %))
          (store--select-statement store-name "SELECT * from kv;")))

(defun store-list-json (store-name)
  (mapcar (| cons (car %) (json-read-from-string (cdr %)))
          (store-list store-name)))

(defun store-list-keys (store-name)
  (mapcar #'first (store--select-statement store-name "SELECT key from kv;")))

(defun store-list-values (store-name)
  (mapcar #'first (store--select-statement store-name "SELECT value from kv;")))

(defun store-list-values-json (store-name)
  (mapcar #'json-read-from-string (store-list-values store-name)))

(provide 'store)
