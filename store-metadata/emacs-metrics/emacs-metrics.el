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
      (let ((result (sqlite3-step stmt)))
        (unless (= sqlite-done result)
          (message "Store %s unable to save function %s: %s" em-store function-name result))))))

(defun emacs-metrics--record-function-call ()
  (when this-command
    (emacs-metrics-save-function-call (symbol-name this-command))))

(defun emacs-metrics-start ()
  (add-hook 'pre-command-hook #'emacs-metrics--record-function-call))

(defun emacs-metrics-stop ()
  (remove-hook 'pre-command-hook #'emacs-metrics--record-function-call))

(provide 'emacs-metrics)
