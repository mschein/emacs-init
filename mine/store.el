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

(defconst store--directory (expand-file-name "~/.emacs-data-store"))


(defun store-get-path (store-name)
  (path-join store--directory (concat store-name ".db")))

(defun store-create-store (store-name &optional schema)
  (ensure-makedirs store--directory)

  (message "Create store at %s" (store-get-path store-name))

  ;; How should migrations work?

  ;;
  ;; Create a directory per store.
  ;;
  ;; (store-create-store store-name 'kv)
  ;;
  ;;
  ;;
  ;; <db-name>/meta.json?
  ;;          /<db-name.db>
  ;;          /migration_<num>.
  ;;



  )

(defun store-exists-p (store-name)
  (ensure-makedirs store--directory)

  (file-exists-p (store-get-path store-name)))

(defun store-remove-store (store-name)
  (ensure-makedirs store--directory)
  (delete-file (store-get-path store-name)))

(defmacro with-store (store-name &rest body)
  `(with-mysqlite3 ((store-get-path ,store-name))
     ,@body))

(provide 'store)
