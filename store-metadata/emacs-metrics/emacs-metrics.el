;;; -*- lexical-binding: t -*-
;;;
;;; Code for database access to the emacs-metrics store.
;;;
(require 'store)
(require 'mysqlite3)

(defconst em-store "emacs-metrics")

(defun emacs-metrics-save-function-call (function-name)
  (with-store em-store
    (sqlite-execute db "INSERT INTO function_metrics(name, count) VALUES (?, 1)
                             ON CONFLICT (name) DO UPDATE SET count=(count + 1)")))

(defun emacs-metrics--record-function-call ()
  (when this-command
    (emacs-metrics-save-function-call (symbol-name this-command))))

(defun emacs-metrics-start ()
  (add-hook 'pre-command-hook #'emacs-metrics--record-function-call))

(defun emacs-metrics-stop ()
  (remove-hook 'pre-command-hook #'emacs-metrics--record-function-call))

(provide 'emacs-metrics)
