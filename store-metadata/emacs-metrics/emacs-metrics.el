;;; -*- lexical-binding: t -*-
;;;
;;; Code for database access to the emacs-metrics store.
;;;
(require 'store)
(require 'mysqlite3)

(defconst em-store "emacs-metrics")

(defun save-function-call (function-name)
  (with-store em-store
    (with-mysqlite3-stmt db "INSERT INTO function_metrics(name, count) VALUES (?, 1)
                             ON CONFLICT (name) DO UPDATE SET count=(count + 1)"
      (sqlite3-bind-multi stmt function-name)
      (unless (= sqlite-done (sqlite3-step stmt))
        (error "Store %s unable to save function %s" em-store function-name)))))

(provide 'emacs-metrics)
