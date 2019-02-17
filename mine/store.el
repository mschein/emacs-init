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

;; How can I tentatively import this?
;; https://github.com/pekingduck/emacs-sqlite3-api
(require 'mysqlite3)

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

(defun store-get-path (store-name)
  "Return the path to the sqlite3 database specified by `store-name'"
  (assert (string-has-value-p store-name))

  (path-join store--directory (concat store-name ".db")))

(defconst store--bare-store-elisp-file ";;; -*- lexical-binding: t -*-
;;;
;;; Code for database access to the %s store.
;;;

(provide '%s)
")

(defun store-create-metadata-dir (metadata-type store-name)
  "Create a basic metadata directory."
  (let* ((metadata-root (ecase metadata-type
                          ('required (first store--metadata-directories))
                          ('local (second store--metadata-directories))))
         (metadata-dir (path-join metadata-root store-name))
         (migration-dir (path-join metadata-dir "migrations")))
    (ensure-makedirs metadata-dir)
    (ensure-makedirs migration-dir)

    (barf (format store--bare-store-elisp-file store-name store-name)
          (path-join metadata-dir (format "%s.el" store-name)))
    (barf (format "-- Initial database creation for store %s" store-name)
          (path-join migration-dir (format "000_%s.sql" store-name)))))


(defun store--get-metadata-version (store-name)
  "Return the current version of a store based on what migrations have been run."
  (with-store store-name
    (with-mysqlite3-stmt db (format "SELECT version from %s where id = 1;" store--migration-table)
      (if (= sqlite-row (sqlite3-step stmt))
          (first (sqlite3-fetch stmt))))))

(defun store--set-metadata-version (store-name version)
  "Update the migration version in the store."
  (with-store store-name
    (with-mysqlite3-stmt db (format "INSERT INTO %s(id, version) VALUES (1, ?)
                                     ON CONFLICT (id) DO UPDATE SET version=excluded.version" store--migration-table)
      (sqlite3-bind-multi stmt version)
      (unless (= sqlite-done (sqlite3-step stmt))
        (error "Store %s unable to set version %s" store-name version)))))

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
    (assert (file-exists-p metadata-dir))
    (assert (file-exists-p migration-dir))

    ;;
    ;; Create the sqlite db, if it doesn't exit
    ;;
    (unless (store-exists-p store-name)
      (message "Create store %s from metadata dir %s" store-name metadata-dir)
      (with-store store-name
                  (sqlite3-exec
                   db
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
                 when (>= (assoc1 :version entry) current-version)
                 do (progn
                      (message "Apply migration %s" (assoc1 :name entry))
                      ;; Can this be one transaction somehow?
                      (do-cmd (list "sqlite3" (store-get-path store-name))
                              :input (assoc1 :name entry))
                      (store--set-metadata-version store-name (assoc1 :version entry))))))))

(defun store--load-metadata (store-name)
  "Load store that uses a migration directory for its schema."
  (cl-loop for metadata-root in store--metadata-directories
           when (file-exists-p (path-join metadata-dir store-name)) do
             (progn
               (store--load-metadata-dir metadata-root store-name)
               (return))))

(cl-defun store-create-store (store-name &key schema use-migration-dir)
  "Setup a store for future use.  Should be run before referencing any store."
  (ensure-makedirs store--directory)

  (message "Create store at %s" (store-get-path store-name))

  (cond
   (schema
     (dolist (entry (to-list schema))
       (with-store store-name
         (sqlite3-exec db entry))))
   (use-migration-dir
     (store--load-metadata store-name))))

(defun store-exists-p (store-name)
  "Does the store name exist already?"
  (ensure-makedirs store--directory)

  (file-exists-p (store-get-path store-name)))

(defun store-remove-store (store-name)
  "Delete a store specified by `store-name'.  This is an unrecoverable delete, so beware."
  (ensure-makedirs store--directory)
  (delete-file (store-get-path store-name)))

(defmacro with-store (store-name &rest body)
  "A macro for running mysqlite functions against a store.
Please use `store-create-store' at some point before calling this function"
  (declare (indent defun))
  `(with-mysqlite3 ((store-get-path ,store-name))
     ,@body))

(provide 'store)
