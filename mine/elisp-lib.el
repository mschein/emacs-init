;; Mike's emacs file.

;;
;; I think it's easier to search longer files, and lisp lends itself well to
;; that.  So for now, one library function, and one company custom file.
;;
;;
;; http://xahlee.org/emacs/elisp_common_functions.html, a good reference.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lisp functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl-lib)

;; Package-Requires
;;
;;
;;

;; Wrap in try catch
(setf lexical-binding t)

(setf directory-sep-char ?/)

(defun aget (key alist)
  "I discovered I was missing this function.
   I'm not entirely sure why I have it, it may
   be worth getting rid of, but there are things that call it.

   Return the cdr in an associated array
   `key`: The key to lookup
   `alist`: The associated list to check.
  "
  (cdr (assoc key alist)))

;; Why doesn't this exist?
(defun printf (fmt &rest args)
  (print (apply #'format fmt args)))

(defmacro* m-when-let ((name test) &rest body)
  "Provides a (when) macro with a let binding.
When-let binds a name to the result of the a test for use inside the BODY.

So (m-when-let (res (+ 1 2 3))
     (print res)
     (- res 1))
returns 5 and prints 6.  res is the name, and (+ 1 2 3) is the
test.  If the test returned nil, then the body will not execute."
  `(let ((,name ,test))
     (when ,name ,@body)))

(defmacro aif (test &rest forms)
  "Anaphoric if that sets the test value to _it_"
  `(let ((_it_ ,test))
     (if _it_ ,@forms)))

(defmacro m-if-let (test-binding &rest forms)
  "Provides an if macro that binds a value a la let.

Example:
 (m-if-let (res (fetch-string))
   (convert-to-something res)
   (error \"failed to fetch string\"))
Here res is the name, and (fetch-string) is the
test clause.  If fetch-string returns nil, the
error case is executed, otherwise the success
case is executed.  res is bount to the result
of the test."
  (destructuring-bind (name test) test-binding
      `(let ((,name ,test))
         (if ,name ,@forms))))

(defmacro if-let* (test-bindings &rest forms)
  "Provides an if-let macro that only works if all lets are true"

  ;; build up a chain of if-lets, with the false case always the same.
  (destructuring-bind (pos &rest neg) forms
    (reduce (fn (prev-bindings binding)
                `(if-let ,binding
                   ,prev-bindings
                   ,@neg))
            (reverse test-bindings)
            :initial-value pos)))

(defmacro when-let* (test-bindings &rest forms)
  ;; Would it be better to do this with a regular loop, to get rid of the
  ;; "car"
  (car
   (reduce (fn (prev-bindings binding)
               `((m-when-let ,binding
                   ,@prev-bindings)))
           (reverse test-bindings)
           :initial-value forms)))

(defun append-if (test lst)
  (m-if-let (res test)
          (append lst (to-list res))))

(defun cons-if (test lst)
  (m-if-let (res test)
          (cons res lst)))

(defun _make-arg-list (arg-alist)
  (let ((out-arg-list '()))
    (dotimes (i (length arg-alist))
      ;; &rest doesn't work yet... how to deal with it?
      (m-if-let (res (assoc (number-to-string  (1+ i)) arg-alist))
       (destructuring-bind (arg . sym) res
         (setf out-arg-list (cons sym out-arg-list)))
       (error "Missing argument %%%s in | form" (1+ i))))
    (reverse out-arg-list)))


;; (defmacro printf (fmt &rest args)
;;   (with-fmt print fmt args))

(defmacro if-test-val (test-fn val &rest forms)
  "run the first argument against val.  If val is true
   provide it to the forms as _it_"
  `(let ((_it_ ,val))
     (if (,test-fn _it_) ,@forms)))

(defmacro if-not (test &rest forms)
  `(if (not ,test) ,@forms))

(defun to-list (thing)
  "Convert thing to a list if it isnt already."
  (if (atom thing) (list thing) thing))

(defun last-car (lst)
  (car (last lst)))

(defun take (n lst)
  (butlast lst (- (length lst) n)))

(defun drop (n lst)
  (last lst (- (length lst) n)))

(defmacro |->> (&rest forms)
  `(| ->> % ,@forms))

(defmacro |-> (&rest forms)
  `(| -> % ,@forms))

(defmacro | (&rest in-forms)
  "A simple way to generate one-off anonymous functions.
   It works just like #() in Clojure (or [] in Arc) and supports
   '%n' and '%&' for &rest.  %n starts at 1, not zero.

   You can say stuff like:
   reduce (| + %1 %2) '(1 2 3)
    -> 6
   mapcar (| `(,%)) '(a b c))
    -> ((a) (b) (c))
   reduce (| apply #'string-join" " %&) '(\"a\" \"b\" \"c\") ->
    \"a b c\""
   ;; Walk the given form looking for '%'s to replace.
   ;; The basic algorithm is each % is replaced with a (gensym) and
   ;; an alist is built up containing: (argpos . gensym) values.
   ;; A single % or the &rest %& are made seperate because they
   ;; require special handling.
   (cl-labels
     ((make-arg-list (arg-alist)
       ;; Converts an alist of (argpos . gensym) pairs into the
       ;; argument list for the function.
       (let ((out-arg-list '()))
         ;; TODO(scheinholtz): Switch to loop, to get rid of the (1+ i)
         (dotimes (i (length arg-alist))
           (let ((arg-pos (1+ i)))
             (m-if-let (res (assoc (number-to-string arg-pos) arg-alist))
                     (destructuring-bind (arg . sym) res
                       (setf out-arg-list (cons sym out-arg-list)))
                     (error "Missing argument %%%s in f form" arg-pos))))
         (reverse out-arg-list)))
      (perc->gensym (elm)
        ;; Given an element in the code forms.  Convert each
        ;; % to its appropriate gensym, and record which argument
        ;; its supposed to be.  %& and % are handled specially.
        (if (symbolp elm)
            ;; TODO(scheinholtz): should I raise an error if I find
            ;; nested '|'s?
            (progn
              (if (equal '| elm)
                  (error "Nested |'s are not allowed."))

            (m-if-let (arg-num (car (string-find "^%\\(.*\\)" (symbol-name elm))))
              (let ((new-name (gensym)))
                ;; TODO(scheinholtz) caseq or case-eq?
                ;; Does that exist?
                (cond
                 ((equal "&" arg-num) (if rest-arg
                                          (setf new-name rest-arg)
                                        (setf rest-arg new-name)))
                 ((equal "" arg-num) (if single-arg
                                         (setf new-name single-arg)
                                       (setf single-arg new-name)))
                 ((equal nil arg-num) (error "Unexpected nil.  Internal Error"))
                 (t (m-if-let (old-sym (aget arg-num alist-args))
                              (setf new-name old-sym)
                              (setf alist-args (acons arg-num new-name alist-args)))))
                new-name)
              elm))
          elm)))

     ;; Bindings for the state produced by the code processing functions.
     (let* ((alist-args '())
            (rest-arg nil)
            (single-arg nil)
            (new-forms (code-walker #'perc->gensym in-forms))
            (arg-list (make-arg-list alist-args)))

       ;; validate arguments and handle single %
       (when single-arg
         (if (or rest-arg alist-args)
             (error "Cannot have %%& or %1-%n with %%"))
         (setf arg-list (list single-arg)))

       `(lambda ,(if rest-arg
                     (append arg-list `(&rest ,rest-arg))
                   arg-list)
          ,@(if (atom (car new-forms))
                `(,new-forms)
              new-forms)))))

(defmacro fn (args &rest forms)
  "Makes a lambda with implicit argument destructuring.

   Checks args for forms.  Anything that is a form is run
   through a destructuring bind, so arguments will be available
   inside the body of the fn.

   Example usage:
    (mapcar (fn ((a . b)) b) '((1 . 2) (3 . 4))) ->
      (2 4)

   Example expansion:
      (fn (((a . b)) (c . d) e f (g h)) ->

       (lambda (gensym gensym e f gensym)
         (destructuring-bind ((a . b)) gensym
          (destructuring-bind (c . d) gensym
           (destructuring-bind (g h) gensym)))
             forms)"
  (let* ((destruct-list '())
         (arg-list (mapcar
                    (lambda (elm)
                      (if (atom elm)
                          elm
                        (progn
                          (let ((sym (gensym)))
                            (setq destruct-list
                                  (cons `(,elm . ,sym) destruct-list))
                            sym))))
                    args)))
    `(lambda ,arg-list
       ;; Build nested destructuring binds.
       ,@(reduce (lambda (forms bind-info)
                   ;; Hackey.  I want the first form to not be a list
                   ;; but the rest of the destructuring-binds should keep
                   ;; their parens.  So I use ,@ and double up the parens
                   ;; on the destruct.
                   `((destructuring-bind ,(car bind-info) ,(cdr bind-info)
                      ,@forms)))
                 destruct-list
                 :initial-value forms))))

(defmacro -> (val &rest forms)
  "From Clojure.  Take val and fill it in as the first argument to
the first form.  The result of that computation is then fed in as
the first argument of the next form, etc. etc.

Example: (-> \"   foo   \" string-trim (concat \"bar\"))
         -> \"foobar\"

The macro expansion is: (concat (string-trim \"  foo  \") \"bar\")"
  (reduce (lambda (accum form)
            (destructuring-bind (head &rest rest) (to-list form)
              (if rest
                  `(,head ,accum ,@rest)
               `(,head ,accum))))
          forms :initial-value val))

(defmacro ->> (val &rest forms)
  "From Clojure.  Like -> except it fills in the last argument
of the form instead of the first.
Example: (->> \"   foo   \" string-trim (concat \"bar\"))
         -> \"barfoo\"

The macro expansion is: (concat \"bar\" (string-trim \"  foo  \"))"

  (reduce (lambda (accum form)
            (destructuring-bind (head &rest rest) (to-list form)
              (if rest
                  `(,head ,@rest ,accum )
               `(,head ,accum))))
          forms :initial-value val))


(defun code-walker (in-fun forms)
  "Call fun on each atom in forms and set the atom to the
result of fun."
  (cl-labels ((mapcar-pairs (mcp-fun forms)
                         (mapcar
                          (lambda (elm-pair)
                            (destructuring-bind (elm last-elm) elm-pair
                              (funcall mcp-fun elm last-elm)))
                          ;; Take a list of elements '(a b c d) and convert it
                          ;; into '((a '...) (b a) (c b) (d c))
                          (reverse (partition 2 (reverse (to-list forms)) :step 1 :pad '...))))
           (quasi-quote-walker (qq-fun forms)
                               (mapcar-pairs
                                (lambda (elm last-elm)
                                  (cond
                                   ((equal '\, last-elm)
                                    (if (atom elm)
                                        (funcall in-fun elm)
                                      (code-walker in-fun elm)))
                                   ((equal '\, elm) elm)
                                   ((atom elm) elm)
                                   (t (quasi-quote-walker in-fun elm))))
                                forms)))
    (mapcar-pairs
     (lambda (elm last-elm)
       (cond
        ((equal '\` last-elm) (quasi-quote-walker in-fun elm))
        ((equal 'quote last-elm) elm)
        ((equal 'quote elm) elm)
        ((equal '\` elm) elm)
        ((atom elm) (funcall in-fun elm))
        (t (code-walker in-fun elm))))
     forms)))

(defun repeat-elm (n elm)
  (if (<= n 0)
      '()
      (cons elm (repeat-elm (1- n) elm))))

(defun* partition (n lst &key (step n) (pad nil))
  "Split a list into sublists of length n.  A step
value is optional.

Example:
 (partition 2 '(1 2 3 4 5) :step 1)
 -> ((1 2) (2 3) (3 4) (4 5) (5))"
  (loop for sublist on lst by
        (lambda (elm) (drop step elm))
        collect (let ((chunk (take n sublist)))
                  (if pad
                      (let ((chunk-len (length chunk)))
                        (if (< chunk-len n)
                            (append chunk (repeat-elm (- n chunk-len) pad))
                          chunk))
                    chunk))))

(defun remove-if-not-regex (regex lst &optional replace)
  (let ((res (remove-if-not (| string-match regex %) lst)))
    (if (string-has-val replace)
        (mapcar (| replace-regexp-in-string regex replace %) res)
      res)))

(defun all-true (lst)
  ;; Note, since or is a macro, we can't do 'or
  (reduce (| and %1 %2) lst))

(defun any-true (lst)
  ;; Note, since or is a macro, we can't do 'or
  (reduce (| or %1 %2) lst))


(defmacro h_ (&rest args)
  "Take list of pairs and convert them in to a hashtable.

Example:
 (h_ "one" 'a "two" 'b)
 -> ... a hash table with one -> a, two -> b"
  (let ((ht (gensym)))
    `(let ((,ht (make-hash-table :test #'equal)))
       ,@(mapcar (lambda (pair)
                   (destructuring-bind (key val) pair
                       `(puthash ,key ,val ,ht)))
                 (partition 2 args))
       ,ht)))

(defun append-if-true (&rest elements)
  "Join the elements into a string if they aren't null
Example:
"
  (interactive)
  (remove-if #'not elements))

(defmacro append-if-passes (alist)
  ;; ((string-is-null-or-empty . val)
  ;;  (>= limit 0) . limit)
  ;;; ...
  `(append-if-true
    ,@(mapcar
       (fn ((test . val))
            `(if ,test ,val))
       alist)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; String functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO(scheinholtz): make a multi arg version
(defun string-join-lst (sep str-list)
  (mapconcat #'identity str-list sep))

(defun string-join (sep &rest args)
  (string-join-lst sep args))

(defun string-split (sep-regex str)
  (split-string str sep-regex))

(defun string-nil-or-empty (s)
  (= 0 (length s)))

(defun string-has-val (s)
  (not (string-nil-or-empty s)))

(defun string-ends-with (str suffix)
  (string-match (concat (regexp-quote suffix) "$") str))

(defun string-starts-with (str prefix)
  (string-match (concat "^" (regexp-quote prefix)) str))

(defun string-or (&rest args)
  ;; Loop through the args, return the first not nil or empty
  ;; string
  (dolist (str args) (if-not (string-nil-or-empty str) (return str))))

(defun string-trim-chars (str front-group back-group)
  (replace-regexp-in-string (format "\\(^[%s]+\\|[%s]+$\\)" front-group back-group) "" str))

(defun string-trim (str)
  "Remove leading and tailing whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))

(defun string-trim-end (str)
  "Remove only the tailing whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "\\([[:space:]\n]*$\\)" "" s)))

;; XXX This doesn't quite work the way I want
;; I want (string-find "\\([0-9+\\)" "1 2 3 4 5") to
;; find all numbers and put them in a list.
(defun* string-find (regex str &optional (start 0))
  (if (string-match regex str start)
      (let ((i 1)
            (out '())
            (matched t))
        (while matched
          (aif (match-string-no-properties i str)
               (progn
                 (setf out (cons  _it_ out)))
               (setf matched nil))
          (setf i (1+ i)))
        (reverse out))))

(defun string-find-all (regex str)
  (let ((index 0)
        (out '()))
    (while (< index (length str))
      (m-if-let (matched (string-find regex str index))
         (progn
           (setq index (match-end 0))
           (setq out (append out matched)))
         (setq index (length str))))
    out))

(defmacro if-string (obj &rest forms)
  "Execute the true form if the string is length > 0"
  `(if (string-has-val ,obj)
       ,@forms))

;; TODO(scheinholtz): Do more with this?
(defalias 'string-replace 'replace-regexp-in-string)


(defun quote-str (str)
  "Quote a string, escaping '\" and \\"
  (apply #'concat
         `("\""
           ,@(mapcar (fn [chr]
                         (let ((s (string chr)))
                           (if (search s "\"\'\\")
                               (concat "\\" s)
                             s)))
                     str)
           "\"")))


(defun string-case= (s1 s2)
  "Compare s1 and s2 ignoring case"
  (string= (downcase s1) (downcase s2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-fmt (fun str &rest args)
  `(,fun (format ,str ,@args)))

;;(defun whocanread (user)
;;  (interactive "suser: ")
;;  (format-wrap 'browse-url-chrome "icanread/

;; TODO(scheinholtz): Use the async I/O for this.
(defun* doit-shell (doit-str silent fmt &rest args)
  (let ((cmd (apply #'format fmt args))
        (doit (string= "yes" doit-str)))
    (when (or (not doit) (not silent))
      (switch-to-buffer "*shell-cmd-buf*")
      (-> cmd (concat "\n") insert)
    (if doit (shell-command cmd)))))

(defun run (&rest cmd-parts)
  (shell-command (combine-and-quote-strings cmd-parts)))

(defun run-to-str (&rest cmd-parts)
  (shell-command-to-string (combine-and-quote-strings cmd-parts)))

;; TODO(scheinholtz): Unify buffer sections.
(defun string->list (str)
  (mapcar #'string-trim (split-string str "\n" t)))

(defun buffer->list ()
  (string->list (buffer-string)))

(defun region->list (begin end)
  (string->list (buffer-substring-no-properties begin end)))

;; do this the right way.
(defalias 'region-to-list 'region->list)

;; TODO(scheinholtz): Do I really need this function?
(defun buffer->list->message ()
  (interactive)
  (->> (buffer->list) (format "%s") message))

(defun buffer->buffer-regex (regex)
  (interactive "sregex: ")
  (list->buffer (remove-if-not-regex regex (buffer->list)) "regex-buffer"))

;;TODO(scheinholtz)
;;
;; The concept of "to" something may be an idiom
;; I can create.
(defun to-buffer (name str)
  (-> name generate-new-buffer set-buffer)
  (insert str))

(defun to-buffer-switch (name str)
  (to-buffer name str)
  (switch-to-buffer name))

(defun to-buffer-switchf (name fmt &rest args)
  (to-buffer-switch name (apply #'format fmt args)))

(defun shell-command-to-buffer (name cmd)
  (to-buffer-switch name (shell-command-to-string cmd)))

(defun shell-command-to-buffer-switch (name cmd)
  (to-buffer-switch name (shell-command-to-string cmd)))

;; TODO(scheinholtz): there must be a better way to do this.
(defun is-y-or-t (c)
  ;; char-equal is case insensitive.
  (any-true (mapcar (| char-equal c %) '(?t ?Y))))

(defun make-shell-buffer-name (path)
  (->> path split-path last-car (format "*sh-%s*")))

(defun with-open-dir (dir handle-fn)
  "Call `handle-fn' with the name of an opened directory and buffer."
  (let ((name (generate-new-buffer-name (make-shell-buffer-name dir))))
    (switch-to-buffer name)
    (cd dir)
    (funcall handle-fn name)))

;; How do I make this switch to the current file directory?
(defun shell-open-dir (dir)
  "Create a shell in the given directory"
  (with-open-dir dir (lambda (name)
                       (shell name))))

(defun shell-dir ()
  "Open a shell in the current default directory"
  (interactive)
  (shell-open-dir default-directory))

(defun eshell-dir ()
  (interactive)
  (with-open-dir (lambda (name)
                   (eshell name)))
  (shell-open-dir default-directory))

(defmacro insertf (fmt &rest args)
  `(with-fmt insert ,fmt ,@args))

(defun browse-url-chrome (url)
  (interactive "surl: ")
  (let ((browse-url-generic-program "/opt/google/chrome/google-chrome"))
    (browse-url-generic url)))

(defun current-line ()
  "Return the line under the cursor, with properties."
  (string-trim-end (thing-at-point 'line)))

(defun duplicate-line ()
  (interactive)
  (end-of-line)
  (open-line 1)
  (beginning-of-line)
  (let ((line (current-line)))
    (next-line)
    (insert line)
    (end-of-line)))

;; (defun search-all-buffers (string)
;;   (multi-isearch-buffers )

;; (defun search-all-buffers-regex (regex)
;;   )

;;
;; TODO(scheinholtz):
;; break a list into (elm, index, first|middle|last) tuples for
;; easier functional processing?
;;
;;(defun list->info-tuple (lst)
;;  (loop )

;; Stolen from http://www.thekidder.com/2008/10/21/emacs-reload-file/
(defun reload-file ()
  (interactive)
  (let ((curr-scroll (window-vscroll)))
    (find-file (buffer-name))
    (set-window-vscroll nil curr-scroll)
    (message "Reloaded file")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Time utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE!!! Bigtable timestamps are msec since epoch!
(defun usec->time (usec)
  (seconds-to-time (/ usec 1000000)))

;; XXX Should also make this run in the buffer (take the last N digits until someting not a number.
(defun usec->date (usec)
  (interactive "dusec: ")
  (format-time-string "%A, %B %e, %Y %r" (usec->time usec)))

;; These are what you want.
(defun current-time-msec ()
  (interactive)
  (-> (current-time) time-to-seconds (* 1000)))

(defun msec->time (msec)
  (seconds-to-time (/ usec 1000)))

(defun msec->date (usec)
  (interactive "dusec: ")
  (-> usec msec->time time->date))

;; XXX Define the conversion macros I have from clojure.
(defun sec->usec (sec)
  (* sec 1000000))

(defun current-time->usec ()
  (-> (current-time) time-to-seconds  sec->usec))

(defun* conv-time (time &key (seconds 0) (minutes 0) (hours 0) (days 0) (months 0) (years 0))
  (let* ((oplist (list seconds minutes hours days months years))
         (split-time (take (length oplist) (decode-time time))))
    (apply #'encode-time (mapcar* '+ split-time oplist))))

(defun last-month (cur-time)
  (time-subtract cur-time (days-to-time 32)))

(defun next-month (cur-time)
  (time-add cur-time (days-to-time 32)))

(defconst week-days '((sun . 0)
                      (mon . 1)
                      (tue . 2)
                      (wed . 3)
                      (thu . 4)
                      (fri . 5)
                      (sat . 6)))

;; sun: 0, mon: 1, Sat: 6
(defun prev-day-of-week (time day-of-week)
  (m-if-let (day-of-week (if (numberp day-of-week)
                             day-of-week
                         (aget day-of-week week-days)))
    (do ((i 1 (+ i 1)))
        ((>= i 8))
      (print i)
      (let ((before (conv-time time :days (- i))))
        ;; The sixth spot is the day of the week.
        (if (= day-of-week (nth 6 (decode-time before)))
            (return before))))))

(defun time->date (time)
  ;; TODO(scheinholtz): there is probably a good letter for this
  ;; instead of listing it out explicitly
  (format-time-string "%A, %B %e, %Y %r" time))

(defun time-split (time elm)
  (let ((time-map '((:year . 5)
                    (:mon-num . 4)
                    (:day . 3)
                    (:hour . 2)
                    (:min . 1)
                    (:sec . 0))))
    (if-let (idx (aget elm time-map))
            (nth idx (decode-time time))
            (raise (format "invalid time element %s" elm)))))

(defun current-time-split (elm)
  (time-split (current-time) elm))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File/Path utils.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun string-remove (pattern str)
  (string-replace pattern "" str))

;; Is this needed?
(defun remove-if-in-set (set seq)
  (remove-if (| find % set :test #'equal) seq))

;; TODO(scheinholtz): Work on me later
;; more loop foo.
(defun tr (char-from char-to str)
  (mapconcat (lambda (elm)
               (if (string= (string elm) char-from)
                   char-to
                 (string elm)))
             str ""))

(defun split-path (path)
  (split-string path (string directory-sep-char) t))

(defun join-path (&rest parts)
  (let ((sep (string directory-sep-char)))
    (reduce (lambda (path elm)
              (if (string-nil-or-empty path)
                  elm
                (concat (string-remove (concat sep "$") path)
                        sep
                        (string-remove (concat "^" sep) elm))))
            parts :initial-value "")))

(defalias 'basename 'file-name-nondirectory)

(defun is-real-file (path)
  (let ((name (basename path)))
    (not (or (string= "." name)
             (string= ".." name)))))

;; find-lisp makes this uneeded.. .keeping for poseterity.
(defun* traverse-directories (cb_fn start_dir &key (file_match nil) (max_depth 500))
  (mapc (lambda (file)
          (if (or (not file_match) (string-match file_match (file-name-nondirectory file)))
              (funcall cb_fn file))
          (if (and (>= max_depth 0) (file-directory-p file))
              (traverse-directories cb_fn file :file_match file_match :max_depth (1- max_depth))))
        (remove-if-not #'is-real-file  (directory-files start_dir t))))

(defun directory-last-dirname (path)
  "Return the name of the last directory in the path.

Example:
  (directory-last-dirname \"/a/b/c.txt\") -> \"b\""
  (-> path directory-file-name split-path last-car))

(defun current-last-dirname ()
  (directory-last-dirname default-directory))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer Utils.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun list->buffer (lst buffer-name)
  (set-buffer (generate-new-buffer buffer-name))
  (insert (string-join-lst "\n" lst)))

;; Use delete-trailing-whitespace to
;; remove whitespace from a file
;; NOTE! delete-blank-lines removes
;; all blank lines, not just the ones at the end.
(defun clean-file ()
  (interactive)
  (delete-trailing-whitespace)
  ;; XXX Delete trailing lines?
  )

(defun buffer-name-no-extn ()
  "Return the buffer name with no extension: a.clj -> a"
  (let ((split-name (string-split "\\." (buffer-name))))
    (if (> (length split-name) 1)
        (string-join-lst "." (butlast split-name 1))
      (car split-name))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insert-interactive ()
  (interactive)
  (let ((pos (point)))
    (re-search-backward "(defun +[^ ]+ +(\\(.*?\\))")
    (m-if-let (arg-str (match-string-no-properties 1))
       (progn
         (printf "arg-str: |%s|\n" arg-str)
         (goto-char pos)
         (insertf "(interactive%s)"
                  (if (string-nil-or-empty arg-str)
                      ""
                    (->> arg-str (string-split " ")
                         (mapcar (| format "s%s: " %)) (string-join-lst "\\n")
                         (format " \"%s\""))))
         (indent-region (line-beginning-position) (line-end-position)))
       (error "unable to parse arglist"))))

(defun set-default-directory (dir)
  (interactive "sdir: ")
  (setq default-directory dir))

(defun jump-to-abbrev (handler abbrev table)
  (if-let* ((abbrev-table-cell
             (remove-if-not
              (fn ((path-abbrev . _))
                  (or (and (= (length abbrev) 1)
                           (string= (-> path-abbrev (elt 0) char-to-string)
                                    abbrev))
                      (string-case= abbrev path-abbrev)))
              table))
            (result-dir (cdar abbrev-table-cell)))
     (funcall handler result-dir)
     (error (format "Invalid abbrev: %s" abbrev))))

(defun jump-to-abbrev-shell (&rest args)
  (apply #'jump-to-abbrev (cons 'shell-open-dir args)))

(defun jump-to-abbrev-dired (&rest args)
  (apply #'jump-to-abbrev (cons 'dired args)))

(defun return-to-pos (fn &rest args)
  "get the current position and pass it to the calling fun.
   Once that function returns, return the cursor to its current position."
  (let ((pos (point)))
    (apply fn (cons pos args))
    (goto-char pos)))

(defun sum-col-region-fn (begin end)
  (save-excursion
    (let* ((lines (region->list begin end))
           (total 0))
      (dolist (line lines)
        (if-let* ((nums (string-find-all "\\(-\\{0,1\\}[0-9]+[0-9\.]*\\)" line))
                  (num-str (car nums))
                  (num (string-to-int num-str)))
          (setq total (+ total num))))
      total)))

(defun sum-col-region (begin end)
  (interactive "r")
  (message "%d" (sum-col-region-fn begin end)))


(defun underline ()
  (interactive)
  (return-to-pos
   (fn (pos)
       )))

  ;; look at the previous line

(defmacro make-bookmark (name url)
  `(defun ,name ()
     (interactive)
     (browse-url-chrome ,url)))

(defun replace-string-in-region (begin end new-string)
  "Given a region defined with begin and end, replace
   That region with the new-string"
  (message (format "got new string: %s" new-string))
  (delete-region begin end)
  (goto-char begin)
  (insert new-string))

(defun tc-str (str)
  (let ()
    (do ((i 0)) ; init
        (< i (length str)) ; end


        )
    ))

(defun title-caps-to-underbar (begin end)
  (interactive "r")
  (let ((case-fold-search nil))
    (replace-string-in-region begin end
      (string-replace
        "\\([A-Z]\\)"
        (| concat "_" (downcase %))
        (buffer-substring-no-properties begin end)))))


(defun linear-regression ()
  ;; http://en.wikipedia.org/wiki/Simple_linear_regression
  )

(defun exponential-moving-avg (smoothing-constant nums)
  (reverse
   (reduce (fn (out-list new)
               (let* ((old (car out-list))
                      (diff (- new old))
                      (update (* (- 1 smoothing-constant) diff)))
                 (cons (+ old update) out-list)))
	   (cdr nums)
           :initial-value (list (car nums)))))

(defun url-join (&rest args)
  (cl-flet ((clean-element (elm)
           (let ((trimmed (string-trim-chars elm "\\/" "\\")))
             (if (string-ends-with elm "://")
                 (concat trimmed "/")
               trimmed))))
    (string-join-lst "/" (mapcar #'clean-element args))))

(defun url-join-lst (elm)
  (apply #'url-join elm))

(defun goto-end-of-buffer ()
  (goto-char (point-max)))

(defun underline ()
  (interactive)
  (save-excursion
    (previous-line)
    (end-of-line)
    (let ((underline-len (current-column)))
      (next-line)
      (beginning-of-line)
      (dotimes (i underline-len)
        (insert "-")))))

(defun multi-occur-all (pattern)
  (interactive "spattern: ")
  (multi-occur (buffer-list) pattern))

(defun elisp-file-to-symbol (file-name)
  (make-symbol (replace-regexp-in-string ".el" "" "linkedin.el")))

(defun yank-to-file-location-python ()
  "Take the path and line number of the current cursor position
and put it into the kill ring.

The idea is that this is an easy way to set a break point in a
python debugging session."
  (interactive)
  (kill-new (format "b %s:%d" (buffer-file-name) (line-number-at-pos))))

(defun clear-buffer (buffer)
  "Clear the contents of the named buffer."
  (with-current-buffer buffer
    (delete-region (point-min) (point-max))))

(defun mount-dmg (path)
  (run "hdiutil" "attach" path))

(defun umount-dmg (path)
  (run "hdiutil" "detach" path))

(defmacro pushd (dir &rest body)
  (let ((old-dir (gensym)))
    `(let ((,old-dir default-directory))
       (unwind-protect
           (progn
             (setf default-directory ,dir)
             ,@body)
         (setf default-directory ,old-dir)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun git-remote-origin-url ()
  (interactive)
  (string-trim (run-to-str "git" "config" "--get" "remote.origin.url")))

(provide 'elisp-lib)
