
;;; ac-nrepl.el --- An auto-complete source for Clojure using nrepl completions

;; Copyright (C) 2012  Steve Purcell <steve@sanityinc.com>

;; Author: Steve Purcell <steve@sanityinc.com>
;; URL: https://github.com/purcell/ac-nrepl
;; Keywords: languages, clojure, nrepl
;; Version: 0.1
;; Package-Requires: ((nrepl "0.1"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Based ac-slime

;;; Installation:

;; Available as a package in both Melpa (recommended) at
;; http://melpa.milkbox.net/ and Marmalade at http://marmalade-repo.org/
;; M-x package-install ac-nrepl

;;; Usage:

;;     (require 'ac-nrepl)
;;     (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
;;     (add-hook 'clojure-nrepl-mode-hook 'ac-nrepl-setup)
;;     (eval-after-load "auto-complete"
;;       '(add-to-list 'ac-modes 'nrepl-mode))


;;; Code:

(require 'nrepl)
(require 'auto-complete)

(defun ac-nrepl-available-p ()
  "Return t if nrepl is available for completion, otherwise nil."
  (not (null (nrepl-current-session))))

;;; TODO: upstream should handle error when current NS is not compiled
(defun ac-nrepl-candidates ()
  (let ((form (format "(complete.core/completions \"%s\" *ns*)" ac-prefix)))
    (car (read-from-string (plist-get (nrepl-send-string-sync form (nrepl-current-ns)) :value)))))

(defun ac-nrepl-documentation (symbol)
  "Return documentation for the given SYMBOL, if available."
  (substring-no-properties
   (replace-regexp-in-string
    "\r" ""
    (replace-regexp-in-string
     "^\\(  \\|-------------------------\r?\n\\)" ""
     (plist-get (nrepl-send-string-sync
                 (format "(clojure.repl/doc %s)" symbol)
                 (nrepl-current-ns))
                :stdout)))))

;;;###autoload
(defface ac-nrepl-candidate-face
  '((t (:inherit ac-candidate-face)))
  "Face for nrepl candidates."
  :group 'auto-complete)

;;;###autoload
(defface ac-nrepl-selection-face
  '((t (:inherit ac-selection-face)))
  "Face for the nrepl selected candidate."
  :group 'auto-complete)

;;;###autoload
(defvar ac-source-nrepl
  '((candidates . ac-nrepl-candidates)
    (available . ac-nrepl-available-p)
    (candidate-face . ac-nrepl-candidate-face)
    (selection-face . ac-nrepl-selection-face)
    (symbol . "n")
    (document . ac-nrepl-documentation))
  "Auto-complete source for nrepl completion.")

;;;###autoload
(defun ac-nrepl-setup ()
  "Add the nrepl completion source to the front of `ac-sources'.
This affects only the current buffer."
  (interactive)
  (add-to-list 'ac-sources 'ac-source-nrepl))

(provide 'ac-nrepl)

;; Local Variables:
;; coding: utf-8
;; eval: (checkdoc-minor-mode 1)
;; End:

;;; ac-nrepl.el ends here

