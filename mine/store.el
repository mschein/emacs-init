;;; -*- lexical-binding: t -*-
;;;
;;; Code for a local store.
;;;
;;; The idea is this should be a generic way to have emacs databases.
;;; It provides a basic key value store and support for sqlite
;;; schemas.
;;;

(require 'cl-lib)
(require 'elisp-lib)

(cl-assert (sqlite-available-p))
(require 'sqlite)

;;
;; what is the goal here.
;; 1. Create a basic "store" management api for dealing
;;    with setting up sqlite databases.
;;
;; 2. Have a migration directory per store that will create
;;    a database based on migration files.
;;
;; TODO(mls) A few open questions:
;; - what should we do about name conflicts?
;; - should db's with different names be able to share the same
;;   migration directory?  Does that make sense?
;;

;;
;; plans:
;; 1. create a commands dir for holding all sql commands in individual files
;; 2. allow for named vars and inheritance
;;

(defconst store--directory (expand-file-name "~/.emacs-data-store"))
(defconst store--metadata-directories
  (mapcar #'expand-file-name '("~/emacs-init/store-metadata"
                               "~/emacs-init/local/store-metadata")))
(defconst store--migration-table "migrations")

;;
;; Metadata dir should contain:
;; /store-metadata/<name>/
;;   -> migrations/
;;      -> <unique-version-number>_<name>.sql
;;   -> <name>.el
;;
(dolist (metadata-dir store--metadata-directories)
  (message "Ensure %s exists" metadata-dir)
  (ensure-makedirs metadata-dir))

(defun store-directory ()
  store--directory)

(defun store-get-path (store-name)
  "Return the path to the sqlite3 database specified by `store-name'"
  (cl-assert (string-has-value-p store-name))

  (path-join store--directory (concat store-name ".db")))

(defconst store--bare-store-elisp-file ";;; -*- lexical-binding: t -*-
;;;
;;; Code for database access to the %s store.
;;;
(require 'store)

;; Use this to access the database
(defconst %s-store \"%s\" \"Constant to use with `with-store' to access the database.\")

;; To start this do: (store-create-store \"%s\" :use-metadata t)

;; Database functions

(provide '%s)
")

(defun store--name-to-root (store-name)
  (cl-loop for root in store--metadata-directories
           with metadata-dir = (path-join root store-name)
           when (file-exists-p metadata-dir)
              return metadata-dir))


(defun store--name-to-dirs (store-name)
  `((:metadata-dir . ,metadata-dir)
    (:migration-dir . ,migration-dir)
    (:sql-dir . ,sql-dir)))

(defun store-create-metadata-dir (metadata-type store-name)
  "Create a basic metadata directory.

  `metadata-type': either 'required or 'local
  `store-name': what the directory will be called.
"
  (let* ((metadata-root (ecase metadata-type
                          (required (cl-first store--metadata-directories))
                          (local (second store--metadata-directories))))
         (metadata-dir (path-join metadata-root store-name))
         (migration-dir (path-join metadata-dir "migrations"))
         (sql-dir (path-join metadata-dir "sql-queries")))

    (ensure-makedirs metadata-dir)
    (ensure-makedirs migration-dir)
    (ensure-makedirs sql-dir)

    ;; Is there a nicer way to do this format string.  Like passing it an alist?
    (barf (format store--bare-store-elisp-file
                  store-name store-name store-name store-name store-name)
          (path-join metadata-dir (format "%s.el" store-name)))
    (barf (format "-- Initial database creation for store %s" store-name)
          (path-join migration-dir (format "000_%s.sql" store-name)))
    metadata-dir))


(defun store--get-metadata-version (store-name)
  "Return the current version of a store based on what migrations have been run."
  (with-store store-name
    (caar (sqlite-select db (format "SELECT version from %s where id = 1;" store--migration-table)))))

(defun store--set-metadata-version (store-name version)
  "Update the migration version in the store."
  (with-store store-name
    (sqlite-execute db (format "INSERT INTO %s(id, version) VALUES (1, ?)
                                ON CONFLICT (id) DO UPDATE SET version=excluded.version" store--migration-table))))

;; TODO(mls): could make the "if regex" thing a macro
(defun store--parse-migration-file-name (name)
  "Split a migration file name into its version and full name."
  (if-let (res (string-find "^\\([0-9]+\\)_.*.sql$" name))
      `((:version . ,(string-to-number (car res)))
        (:name . ,name))
    (error "Failed to parse name: %s" name)))

(defun store--load-metadata-dir (metadata-root store-name)
  "Setup a database and run any needed migrations."
  (let* ((metadata-dir (path-join metadata-root store-name))
         (migration-dir (path-join metadata-dir "migrations")))
    ;; Make sure this is a valid metadata directory.
    (cl-assert (file-exists-p metadata-dir))
    (cl-assert (file-exists-p migration-dir))

    ;;
    ;; Create the sqlite db, if it doesn't exit
    ;;
    (unless (store-exists-p store-name)
      (message "Create store %s from metadata dir %s" store-name metadata-dir)
      (with-store store-name
        (sqlite-execute db
         (format "CREATE TABLE %s (id Integer PRIMARY KEY, version Integer);"
                 store--migration-table))))

    ;;
    ;; Run the migrations on the database.
    ;;

    (message "Run migration dir %s" migration-dir)

    ;; What version is the database one?
    (let* ((current-version (store--get-metadata-version store-name))
           (current-version (or current-version -1)))

      (message "Current store version %s for store %s" current-version (store-get-path store-name))

      ;; read the migration files
      (pushd migration-dir
        (cl-loop for entry in (mapcar #'store--parse-migration-file-name
                                      (list-directory-entries migration-dir))
                 when (> (assoc1 :version entry) current-version)
                 do (progn
                      (message "Apply migration %s" (assoc1 :name entry))
                      ;; Can this be one transaction somehow?
                      (do-cmd (list "sqlite3" (store-get-path store-name))
                              :input (assoc1 :name entry)
                              :throw t)
                      (store--set-metadata-version store-name (assoc1 :version entry))))))
    ;; Load the db function
    (load (path-join metadata-dir store-name))))

(defun store--load-metadata (store-name)
  "Load store that uses a migration directory for its schema."
  (cl-loop for metadata-root in store--metadata-directories
           when (file-exists-p (path-join metadata-root store-name)) do
             (progn
               (store--load-metadata-dir metadata-root store-name)
               (return))))

(defun store--load-sql-query (store-name query-name)
  (let ((sql-query-file (path-join )))))

(cl-defun store-create-store (store-name &key schema use-metadata)
  "Setup a store for future use.  Should be run before referencing any store."
  (ensure-makedirs store--directory)

  (message "Create store at %s" (store-get-path store-name))

  (cond
   (schema
     (dolist (entry (to-list schema))
       (with-store store-name
         (sqlite-execute db entry))))
   (use-metadata
     (store--load-metadata store-name))))

(defun store-exists-p (store-name)
  "Does the store name exist already?"
  (ensure-makedirs store--directory)

  (let ((path (store-get-path store-name)))
  (and (file-exists-p path)
       (file-has-size-p path))))

(defun --confirm-delete-file (file)
  (when (and (file-exists-p file)
           (y-or-n-p (format "Delete file %s:" file)))
    (delete-file file)))

(cl-defun store-remove-store (store-name &key all)
  "Delete a store specified by `store-name'.  This is an unrecoverable delete, so beware."
  (ensure-makedirs store--directory)
  (cl-flet ((make-file-name (index)
              (format "%s.%d" (store-get-path store-name) index)))
    (if all
        (progn
          (--confirm-delete-file (store-get-path store-name))
          (cl-loop for x from 1
                   if (file-exists-p (make-file-name x))
                      do (--confirm-delete-file (make-file-name x))
                   else
                   return t))
      (cl-loop for x from 1
               when (not (file-exists-p (make-file-name x)))
                 do (progn (rename-file (store-get-path store-name) (make-file-name x))
                           (cl-return))))))

(defun store--set-sqlite-pragmas (db)
  (sqlite-pragma db "PRAGMA journal_mode=WAL;")
  (sqlite-pragma db "PRAGMA sysynchronous = NORMAL;")
  (sqlite-pragma db "PRAGMA foreign_keys = ON;")
  (sqlite-pragma db "PRAGMA journal_size_limit = 67108864 -- 64 megabytes;"))

(defmacro with-store (store-name &rest body)
  "A macro for running sqlite functions against a store.  The idea
is to have a convienient place/way to store the database my emacs uses for
its operations.  Note that store-name can be an open sqlite connection or
a string designating a store.

Please use `store-create-store' at some point before calling this function.

Creates the variable `db' for access to the sqlite database.

`store-name': sqlite connection or store name string.
"
  (declare (indent defun))
  (with-gensyms (gstore gis-open-db)
    `(let* ((,gstore ,store-name)
            (,gis-open-db (sqlitep ,gstore))
            (db (if ,gis-open-db
                    ,gstore
                  (sqlite-open (store-get-path ,gstore)))))
       (unwind-protect
           (progn
             (store--set-sqlite-pragmas db)
             ,@body)
         (when (and db
                    (not ,gis-open-db))
           (message "Close sqlite conn %s" db)
           (sqlite-close db))))))

(defmacro with-store-txn (store-name &rest body)
  "A macro to access a store within a sqlite write transaction.  Note that
if this is too broad, you can call `with-sqlite-transaction' directly.

`store-name': sqlite connection or store name string.
"
  (declare (indent defun))
  `(with-store ,store-name
     (with-s\qlite-transaction db
       ,@body)))

(cl-defmacro with-store-select-set ((store-name query &optional args) &rest body)
  "A with- macro to allow for cleanup of sqlite statements."
  (declare (indent defun))

  `(with-store ,store-name
     (let ((stmt (sqlite-select db ,query ,args 'set)))
       (unwind-protect
           (progn
             ,@body)
         (when stmt
           (sqlite-finalize stmt))))))


(provide 'store)
