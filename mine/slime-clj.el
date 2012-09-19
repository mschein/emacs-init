(add-to-list 'load-path "~/.emacs.d/swank-clojure")
(require 'clojure)
(require 'swank-clojure)

(defun slime-connect-clj ()
  (interactive)
  ;; TODO: figure out a way to autoload util.clj
  (slime-connect "127.0.0.1" 4005))

(global-set-key "\C-c\M-s"  'start-slime)

;(slime-connect-clj)

(provide 'slime-clj)
