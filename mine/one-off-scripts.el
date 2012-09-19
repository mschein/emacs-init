;;
;; This file contains useful emacs lisp scripts I've come up with that
;; may not ever get used more then a few times.
;;
;; Scribbling should go here.
;;

(require 'elisp-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Personal One Offs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ride-home ()
  (interactive)
  (browse-url-chrome "http://maps.google.com/maps?f=q&source=s_q&hl=en&geocode=&q=san+jose+ca&sll=37.339386,-121.894956&sspn=0.444376,0.910492&g=san+jose&ie=UTF8&hq=&hnear=San+Jose,+Santa+Clara,+California&ll=37.378888,-121.889191&spn=0.891557,2.602386&z=10&layer=t&err=1"))


;; (defun make-abbrev-list (lst)
;;   "Convert a list of strings into an alist of abbreviations for those strings
;; use the first letter of each word and just the first letter, if it
;; is unique."
;;   (reduce
;;    (flatten
;;    (mapcar (lambda (name)
;;
;;              ((replace-regexp-in-string
;;                       "\\(\\w+\\)\\W*"
;;                       (lambda (word)
;;                         (string (aref word 0)))
;;                       name))
;;              )
;;            lst)
;;   )

(defun list-fonts ()
  (interactive)
  (shell-command-to-buffer-switch "*all-fonts*" "xlsfonts"))

(defun show-font ()
  (interactive)
  (format-wrap shell-command "xfd -fn \'%s\'" (current-line)))


;; Not sure how useful this is, so I'll leave it here.
(defun* new-file-contents (path new-contents)
  (find-file path)
  (erase-buffer)
  (goto-char 0)
  (insert new-contents))


(defun open-todo ()
  (interactive)
  (find-file-noselect "~/doc/todo.org")
  (switch-to-buffer-other-window "todo.org"))
  

(defun find-dup (lst)
  ;; loop though list
  ;; get element at random length
  ;; how do you know which ones to
)

(defun slope (v1 v2)
  (/ (- (cadr v1) (cadr v2))
     (- (car v1) (car v2))))
 

(provide 'one-off-scripts)
