;;; ac-nrepl-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (ac-nrepl-setup) "ac-nrepl" "ac-nrepl.el" (20548
;;;;;;  22014))
;;; Generated autoloads from ac-nrepl.el

(defface ac-nrepl-candidate-face '((t (:inherit ac-candidate-face))) "\
Face for nrepl candidates." :group (quote auto-complete))

(defface ac-nrepl-selection-face '((t (:inherit ac-selection-face))) "\
Face for the nrepl selected candidate." :group (quote auto-complete))

(defvar ac-source-nrepl '((candidates . ac-nrepl-candidates) (available . ac-nrepl-available-p) (candidate-face . ac-nrepl-candidate-face) (selection-face . ac-nrepl-selection-face) (symbol . "n") (document . ac-nrepl-documentation)) "\
Auto-complete source for nrepl completion.")

(autoload 'ac-nrepl-setup "ac-nrepl" "\
Add the nrepl completion source to the front of `ac-sources'.
This affects only the current buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("ac-nrepl-pkg.el") (20548 22014 769764))

;;;***

(provide 'ac-nrepl-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ac-nrepl-autoloads.el ends here
