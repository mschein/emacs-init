;;
;; This should be replaced with nrepl
;;

;; (condition-case nil
;;     (unload-feature 'swank-clojure)
;;   (error nil))
;; (condition-case nil
;;     (unload-feature 'slime)
;;   (error nil))

;; (add-to-list 'load-path "~/.emacs.d/swank-clojure")
;; ;; XXX This should not be needed
;; ;(add-to-list 'load-path "~/.emacs.d/elpa/clojure-mode-1.7.1/clojure-mode.el")

;; ;; Point to the crappy slime.
;; (setq load-path (remove-if (| string-match "slime" %) load-path))
;; (add-to-list 'load-path "~/.emacs.d/elpa/slime-20100404")
;; (add-to-list 'load-path "~/.emacs.d/elpa/slime-repl-20100404")

;; ;; sets up everything
;; (require 'swank-clojure)

;; (defun slime-connect-clj ()
;;   (interactive)
;;   ;; TODO: figure out a way to autoload util.clj
;;   (slime-connect "127.0.0.1" 4005))

;; (global-set-key "\C-c\M-s"  'start-slime)

;; ;(slime-connect-clj)

;; (provide 'slime-clj)
