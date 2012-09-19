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
;;     (progn
;;       (unload-feature 'swank-clojure)
;;       (unload-feature 'slime))
;;   (+ 1 2))

;(setq inferior-lisp-program "~/bin/ccl64")
(setq load-path (remove-if (| string-match "slime" %) load-path))
(add-to-list 'load-path "~/.emacs.d/slime-3.0/slime-2010-11-21/")  ; your SLIME directory
(require 'slime)
(slime-setup '(slime-fancy))
(setq slime-multiprocessing t)
(setq slime-lisp-implementations
      ;;~/Documents/dev/src/contrib/clozure/ccl-1.6/scripts/ccl64
      '((ccl ("~/bin/ccl64"))
        ;; find a more up to date sbcl.
        (sbcl ("/usr/local/bin/sbcl") :coding-system utf-8-unix)))
(setf slime-default-lisp 'ccl)

(provide 'slime-lisp)


;; unused.
;(add-hook ’lisp-mode-hook (lambda () (slime-mode t)))
;(add-hook ’inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
;
;;(slime-setup)
