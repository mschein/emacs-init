;;; daemons-brew.el --- UI for managing homebrew services -*- lexical-binding: t -*-

;; Copyright (c) 2018 Steve Purcell
;;
;; Author: Steve Purcell
;; URL: https://github.com/cbowdon/daemons.el
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;; Created: Mar 10, 2018
;; Modified: Mar 10, 2018
;; Version: 1.1.0
;; Keywords: unix convenience
;; Package-Requires: ((emacs "25"))
;;
;;; Commentary:
;; This file provides support for the MacOS "brew services" tool to
;; daemons.el.

;;; Code:
(require 'seq)
(require 'daemons)

(defvar daemons-brew--commands-alist
  '((status . (lambda (name) (format "brew services list %s" name)))
    (start . (lambda (name) (format "brew services start %s" name)))
    (stop . (lambda (name) (format "brew services stop %s" name)))
    (restart . (lambda (name) (format "brew services restart %s" name))))
  "Daemons commands alist for Brew.")

(defun daemons-brew--parse-list-item (output)
  "Parse a single line from OUTPUT into a tabulated list item."
  (let* ((parts (split-string output nil t))
         (name (nth 0 parts))
         (status (nth 1 parts))
         (user (nth 2 parts))
         (plist (nth 3 parts)))
    (list name (vector name status (or user "") (or plist "")))))

(defun daemons-brew--list ()
  "Return a list of daemons on a Brew system."
  (thread-last "brew services list"
    (daemons--shell-command-to-string)
    (daemons--split-lines)
    (cdr)
    (seq-map 'daemons-brew--parse-list-item)))

(defun daemons-brew--list-headers ()
  "Return the list of headers for a Brew ‘daemons-mode’ buffer."
  (apply 'vector
         (list '("Daemon (service)" 40 t)
               '("Status" 10 t)
               '("User" 10 t)
               '("Plist" 50 t))))

(setq daemons--commands-alist daemons-brew--commands-alist
      daemons--list-fun 'daemons-brew--list
      daemons--list-headers-fun 'daemons-brew--list-headers)

(provide 'daemons-brew)
;;; daemons-brew.el ends here
