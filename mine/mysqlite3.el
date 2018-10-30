;; How can I tentatively import this?
;; https://github.com/pekingduck/emacs-sqlite3-api
;;
;; also:
;; https://www.sqlite.org/rescode.html  -- constants
;; https://www.sqlite.org/c3ref/intro.html  -- api reference
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

(defmacro with-mysqlite3-txn (open-args &rest body)
  (declare (indent defun))

  `(with-mysqlite3 ,open-args
     (sqlite3-exec db "BEGIN")
     ,@body
     (sqlite3-exec db "COMMIT")))

;; We need this, because we need to free stmts
(defmacro with-mysqlite3-stmt (db sql-text &rest body)
  (declare (indent defun))

  `(let ((stmt (sqlite3-prepare ,db ,sql-text)))
     (unwind-protect
         (progn
           ,@body)
       (when stmt
         (sqlite3-finalize stmt)))))


(defun mysqlite3-query ())


(provide 'mysqlite3)
