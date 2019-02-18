;;; -*- lexical-binding: t -*-
;;;
;;; Code for database access to the emacs-metrics store.
;;;
(require 'store)
(require 'mysqlite3)

(defconst em-store "emacs-metrics")

(defun emacs-metrics-save-function-call (function-name)
  (with-store em-store
    (with-mysqlite3-stmt db "INSERT INTO function_metrics(name, count) VALUES (?, 1)
                             ON CONFLICT (name) DO UPDATE SET count=(count + 1)"
      (sqlite3-bind-multi stmt function-name)
      (unless (= sqlite-done (sqlite3-step stmt))
        (error "Store %s unable to save function %s" em-store function-name)))))

(defun emacs-metrics--record-function-call ()
  (emacs-metrics-save-function-call (symbol-name this-command)))

(defun emacs-metrics-start ()
  (add-hook 'post-command-hook #'emacs-metrics--record-function-call))

(defun emacs-metrics-stop ()
  (remove-hook 'post-command-hook #'emacs-metrics--record-function-call))

(provide 'emacs-metrics)
