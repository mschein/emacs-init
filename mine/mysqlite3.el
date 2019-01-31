;; How can I tentatively import this?
;; https://github.com/pekingduck/emacs-sqlite3-api
;;
;; also:
;; https://www.sqlite.org/rescode.html  -- constants
;; https://www.sqlite.org/c3ref/intro.html  -- api reference
;; https://github.com/pekingduck/emacs-sqlite3-api
;;
(require 'sqlite3-api)


;; Stuff needed
;;
;; with-sqlite
;;
;; open read or write or both
;;
;; stuff for fetching things
;;
;; with-txn
;;

(cl-defun mysqlite3-open (db-path &key readonly)
  (let ((flag (if readonly sqlite-open-readonly sqlite-open-readwrite)))
    (sqlite3-open db-path flag sqlite-open-create)))

(defmacro with-mysqlite3 (open-args &rest body)
  (declare (indent defun))

  `(let ((db (mysqlite3-open ,@open-args)))
     (unwind-protect
         (progn
           ,@body)
       (when db
         (sqlite3-close db)))))

(defmacro with-mysqlite3-txn (db &rest body)
  (declare (indent defun))

  `(unwind-protect
         (progn
           (sqlite3-exec ,db "BEGIN")
           ,@body
           (sqlite3-exec ,db "COMMIT"))
       (sqlite3-exec ,db "ROLLBACK")))

;; We need this, because we need to free stmts
(defmacro with-mysqlite3-stmt (db sql-text &rest body)
  (declare (indent defun))

  `(let ((stmt (sqlite3-prepare ,db ,sql-text)))
     (unwind-protect
         (progn
           ,@body)
       (when stmt
         (sqlite3-finalize stmt)))))

(defun mysqlite3-create-index-str (table-name field &optional unique)
  (format "CREATE %sINDEX %s_%s_index ON %s (%s)"
          (if unique "UNIQUE " "")
          table-name field table-name field))

(defun mysqlite3-create-db-schema (path schema)
  (with-mysqlite3 path
    (sqlite3-exec db schema)))

(defun mysqlite3-create-db (path &rest schema-list)
  (mysqlite3-create-db-schema path (string-join schema-list "\n")))

(defun mysqlite3-stmt (cmd &rest bindings)
  )

;;
;;
;; How do I want to specify the db + transactions?
;;  (with-txn )
;;

(cl-defun mysqlite3-cmd (cmd bindings &key read-only)
  (with-mysqlite3 (*db-path* :read-only read-only)
    (with-mysqlite3-stmt db cmd ))
  )

(defun mysqlite3-select (cmd bindings &optional columns)
  (with-))
(defun mysqlite3-insert (cmd values))
(defun mysqlite3-update (cmd values))
(defun mysqlite3-delete (cmd bindings))

;; Can I do this through the api?
(defun mysqlite3-parse-schema ())

;;
;; New interface ideas.
;; 1. Extend store to include:
;;    - Keep a mapping of database name to connection information
;;      - most likely a path to a sqlalchemy file, but I could also
;;        connect to a postgres server or something.
;;    - Keep a migrations dir under the source tree.
;; 1. Use a dynamic variables for the db path/connection
;; 2. Use a macro with symbol-macrolet to create commands.
;;
;;
;; (with-db <name>
;;    (fetch-one-or-error (cmd "SELECT * from table")))
;;
;; (with-db <name>
;;    ()
;;
;; Key value store code ideas:
;;   (let ((+db-name+ <name for store>))
;;
;;
;;
;;
;;
;; How should migrations work:
;;  - create a directory somewhere
;;  - contains files
;;    -  <version>_<upgrade|downgrade>_<description>.sql
;;
;; other options:
;; <dir>/migrations.el
;;      /<migration_files.sql>
;;
;; migrations.el:
;;   '(((version . 1)
;;      (description . "Text")
;;      (upgrade . "<upgrade file name>")
;;      (downgrade . "downgrade file name>"))
;;     (vesion . 2)
;;      (description . "Text")
;;      (upgrade . "<upgrade file name>")
;;      (downgrade . "downgrade file name>")))
;;

(defun mysqlite3-migration ())

(provide 'mysqlite3)
