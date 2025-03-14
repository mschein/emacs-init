;;
;; How to use:
;; (load slime- () lisp)
;; M-- M-x slime
;; slime start ccl.
;;
;;

;; Remove the other slime directory
;; This doesn't work reliably... but it can
;; be run to unload crappy slime.
;; (unwind-protect
;;    (unload-feature 'swank-clojure)
;;  (message "No need to unload swank clojure"))

;; (unwind-protect
;;     (unload-feature 'slime)
;;   (message "No need to unload slime"))

(require 'elisp-lib)

;; These seem to work better.  Why?
(condition-case nil
    (unload-feature 'swank-clojure)
  (error nil))
(condition-case nil
    (unload-feature 'slime)
  (error nil))

;;(add-to-list 'load-path "~/emacs-init/slime")  ; your SLIME directory

(when (file-exists-p "~/quicklisp/slime-helper.el")
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (require 'slime)
  (require 'slime-autoloads)
  (slime-setup '(slime-fancy)))


;;
;; To use this do: M - M-x slime
;;
(setq slime-lisp-implementations
      '((sbcl ("sbcl" "--dynamic-space-size" "20480"))
        (ccl ("ccl64" "--quiet"))
        (abcl ("abcl"))))
(setq load-path (cl-remove-if (| string-match "slime" %) load-path))

;; Set the common lisp hyperspec root

;; To get the common lisp hyperspec locally
;; 1. (ql:quickload "clhs")
(load (format "/Users/%s/quicklisp/clhs-use-local.el" (user-full-name)) t)

;; This doesn't work that well...
;; (require 'info-look)
;; (info-lookup-add-help
;;  :mode 'lisp-mode
;;  :regexp "[^][()'\" \t\n]+"
;;  :ignore-case t
;;  :doc-spec '(("(ansicl)Symbol Index" nil nil nil)))


;(setq slime-multiprocessing t)
(provide 'slime-lisp)

;; unused.
;(add-hook ’lisp-mode-hook (lambda () (slime-mode t)))
;(add-hook ’inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
