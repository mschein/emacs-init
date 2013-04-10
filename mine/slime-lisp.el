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

;; These seem to work better.  Why?
(condition-case nil
    (unload-feature 'swank-clojure)
  (error nil))
(condition-case nil
    (unload-feature 'slime)
  (error nil))

(setq inferior-lisp-program "ccl64")
(setq load-path (remove-if (| string-match "slime" %) load-path))
(add-to-list 'load-path "~/emacs-init/slime")  ; your SLIME directory

(require 'slime)
(require 'slime-autoloads)
(slime-setup '(slime-fancy))
(load (expand-file-name "~/quicklisp/slime-helper.el"))

;(setq slime-multiprocessing t)
(provide 'slime-lisp)

;; unused.
;(add-hook ’lisp-mode-hook (lambda () (slime-mode t)))
;(add-hook ’inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
