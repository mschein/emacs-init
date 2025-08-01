;; -*- lexical-binding: t -*-
;; Mike's emacs file.

;;
;; I think it's easier to search longer files, and lisp lends itself well to
;; that.  So for now, one library function, and one company custom file.
;;
;;
;; http://xahlee.org/emacs/elisp_common_functions.html, a good reference.
;;
;; Also:
;; https://github.com/alphapapa/emacs-package-dev-handbook
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lisp functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl-lib)
(require 'f)
(require 'ht)
(require 'json)
(require 's)
(require 'subr-x)
(require 'uuidgen)


(defconst directory-sep "/")

(defalias 'filter #'cl-remove-if-not)

(defmacro pushcons (key value alist)
  "Push a new key value pair onto an alist."
  `(push (cons ,key ,value) ,alist))

(defun sum (elms)
  "Sum a sequence named `ELMS'."
  (if (vectorp elms)
      (cl-loop for elm across elms sum elm)
    (cl-loop for elm in elms sum elm)))

(unless (fboundp 'gensym)
  (let ((my-gensym-counter 0))
    (defun gensym ()
      "Return an uninterned symbol."
      (make-symbol (format "G%d" (cl-incf my-gensym-counter))))))

(defmacro with-gensyms (syms &rest body)
  (declare (indent defun))
  `(let ,(mapcar (| list % '(gensym)) syms)
     ,@body))

(defun --assoc1-common (keys list)
  "Don't call this directly usually.  It's here to make
the setter work."
  (let ((keys (to-list keys))
        out)
    (cl-flet ((as (key al)
                  (if-let (answer (assoc key al))
                      (cdr (setf out answer))
                    (error "Key \'%s\' not found" key))))

      (while keys
        (let ((res (as (car keys) list)))
          (setf list res)
          (setf keys (cdr keys)))))
    out))

(defun assoc1 (&rest args)
  "Lookup a key (or keys) in an alist and raise an error if its not there.

   Returns the value (The cdr of the element).
   Values can be nil and it will still work.

   Also note that you can traverse nested alists by providing a list of keys.

   So (assoc1 '(a b c) '((a . ((b . ((c . 1))))))) -> 1

   `keys': The key or a list of keys to lookup.
   `list': The associated list to check.
  "
  (cdr (apply #'--assoc1-common args)))

(defun rassoc1 (value list)
  "Lookup a key in an alist matching the cdr and raise an error if its not there.

   Returns the value (The car of the element).
   Values can be nil and it will still work.
   `key': The key to lookup
   `list': The associated list to check.
  "

  (if-let (answer (rassoc value list))
      (car answer)
    (error "Value \'%s\' not found" value)))

;; Note, this only works in emacs 26
;; (defun assoc-get (key alist &optional default)
;;   (alist-get key alist default nil #'equal))

;; Do this by hand until I stop needing emacs 25.
(defun assoc-get (key alist &optional default)
  (if-let (value (assoc key alist))
      (cdr value)
    default))

(defun assoc-keys (alist)
  "Return all of the keys in an alist.  That is the `car' values."
  (mapcar #'car alist))

(defun assoc-values (alist)
  "Return all of the values in an alist.  That is the `cdr' values."
  (mapcar #'cdr alist))

(defun assoc-uniqify (alist)
  "Return an alist with only unique keys."
  (cl-loop for (k . v) in alist
           with h = (ht)
           unless (ht-contains? h k)
           collect (cons k v)
           do (ht-set h k t)))

(defun assoc1-traverse (steps data)
  "Traverse a series of alists and arrays as returned by aws and
   emacs's json parser.

   The caller can specify an integer for the array/list index,
   a string/symbol for an alist, and a function for anything else needed.

   Example: (aws-traverse '(Reservations 0 Instances (fn) 0)
                           (aws-ec2-describe-instances instance-ids))"

  (cl-loop for step in steps
           with data = data do
     (etypecase step
       (integer (setf data (elt data step)))
       (symbol (setf data (assoc1 step data)))
       ;; Originally I used "function" here, but
       ;; it doesn't play well with "symbol", so now
       ;; just use a list to indicate a function call.
       (list (setf data (funcall (car step) data)))
       (string (setf data (assoc1 step data))))
     finally return data))

;; As I used this more, it can become more DSL like...
(cl-defun assoc1-to-assoc (mappings data &key (throw t))
  "Translate an alist or json blob into a new alist.

  `mappings' should be an alist that contains:
             ((<name> . (steps to pass to `assoc1-traverse')

  `data': The structure to remap."
  (cl-loop for (name . steps) in mappings
           for value = (cl-flet ((translate-fn ()
                                   (assoc1-traverse steps data)))
                         (if throw
                             (translate-fn)
                           (ignore-errors (translate-fn))))
           collect (cons name value)))

(defun assoc1-filter (keys-to-keep alist)
  "Translate an alist into a smaller alist
   that only has keys in `KEYS-TO-KEEP'."
  (cl-loop for key in keys-to-keep
           collect (cons key (assoc1 key alist))))

(defun assoc-to-yaml (alist)
  (string-join (mapcar (fn ((key . value))
                         (if (atom value)
                             (format "%s: %s" key value)
                           (assoc-to-yaml value)))
                       alist)
               "\n"))

(defun assoc-group-dups (alist)
  (let (out)
    (cl-loop for (k . v) in alist
             do (if-let (cel (assoc k out))
                    (setcdr cel (cons v (to-list (cdr cel))))
                 (pushcons k v out)))
    (reverse out)))

;; Make it so you can use assoc1 with setf.
(gv-define-setter assoc1 (value &rest args) `(setcdr (--assoc1-common ,@args) ,value))

(defun identity (arg)
  arg)

(defun unique (seq)
  "return a copy of the sequence with only unique elements"
  (delete-dups (mapcar #'identity seq)))

;;
;; This was more annoying than I expected.
;; you lose the form, and it doesn't seem like
;; assert is meant to build on.
;;
;; (defmacro assert-value (form &rest args)
;;   (with-gensyms (value)
;;     `(let ((,value ,form))
;;        (assert ,value ,@args)
;;        ,value)))

;; Why doesn't this exist?
(defun printf (fmt &rest args)
  (print (apply #'format fmt args)))

;;
;; The macros use string-find, so keep it here.
;;
;; XXX This doesn't quite work the way I want
;; I want (string-find "\\([0-9+\\)" "1 2 3 4 5") to
;; find all numbers and put them in a list.
;;
(cl-defun string-find (regex str &optional (start 0))
  (if (string-match regex str start)
      (let ((i 1)
            (out '())
            (matched t))
        (while matched
          (if-let (it (match-string-no-properties i str))
              (progn
                (setf out (cons  it out)))
            (setf matched nil))
          (setf i (1+ i)))
        (reverse out))))

(defun string-find-all (regex str)
  (let ((index 0)
        (out '()))
    (while (< index (length str))
      (if-let (matched (string-find regex str index))
         (progn
           (setq index (match-end 0))
           (setq out (append out matched)))
         (setq index (length str))))
    out))

;; (unless (fboundp 'assert)
;;   (defalias 'assert #'cl-assert))

;; (unless (fboundp 'destructuring-bind)
;;   (defalias 'destructuring-bind #'cl-destructuring-bind))

;; (unless (fboundp 'loop)
;;   (defalias 'loop #'cl-loop))

;; (unless (fboundp 'reduce)
;;   (defalias 'reduce #'cl-reduce))

;; (unless (fboundp 'remove-if)
;;   (defalias 'remove-if #'remove-if))

;; Keep some of the (require 'cl) functions
(cl-loop for (sym cl-sym) in '((assert cl-assert)
                               (destructuring-bind cl-destructuring-bind)
                               (loop cl-loop)
                               (reduce cl-reduce)
                               (remove-if cl-remove-if))
         do (unless (fboundp sym)
              (defalias sym cl-sym)))

(unless (fboundp 'if-let)
  (defmacro if-let (test-binding &rest forms)
    "Provides an if macro that binds a value a la let.

     Example:
      (if-let (res (fetch-string))
        (convert-to-something res)
        (error \"failed to fetch string\"))
     Here res is the name, and (fetch-string) is the
     test clause.  If fetch-string returns nil, the
     error case is executed, otherwise the success
     case is executed.  res is bount to the result
     of the test."
    (destructuring-bind (name test) test-binding
      `(let ((,name ,test))
         (if ,name ,@forms)))))

(unless (fboundp 'if-let*)
  (defmacro if-let* (test-bindings &rest forms)
    "Provides an if-let macro that only works if all lets are true"

    ;; build up a chain of if-lets, with the false case always the same.
    (destructuring-bind (pos &rest neg) forms
      (cl-reduce (fn (prev-bindings binding)
                   `(if-let ,binding
                        ,prev-bindings
                      ,@neg))
                 (reverse test-bindings)
                 :initial-value pos))))

(unless (fboundp 'when-let*)
  (defmacro when-let* (test-bindings &rest forms)
    ;; Would it be better to do this with a regular loop, to get rid of the
    ;; "car"
    (declare (indent defun))
    (car
     (cl-reduce (fn (prev-bindings binding)
                  `((when-let ,binding
                        ,@prev-bindings)))
                (reverse test-bindings)
                :initial-value forms))))

(unless (fboundp 'acond)
  (defmacro acond (&rest clauses)
    "An anaphoric cond.  Each test binds its result to it
  for use in the corresponding clause."
    (cl-labels ((process (clauses)
                  (when clauses
                    (destructuring-bind ((test &rest body) . rest) clauses
                      `(if-let (it ,test)
                           (progn
                             ,@body)
                         ,(process rest))))))
      (process clauses))))

(defun append-if (test list)
  (if-let (res test)
          (append list (to-list res))))

(defun cons-if (test list)
  (if-let (res test)
          (cons res list)))

(defun _make-arg-list (arg-alist)
  (let ((out-arg-list '()))
    (dotimes (i (length arg-alist))
      ;; &rest doesn't work yet... how to deal with it?
      (if-let (res (assoc (number-to-string  (1+ i)) arg-alist))
       (destructuring-bind (arg . sym) res
         (setf out-arg-list (cons sym out-arg-list)))
       (error "Missing argument %%%s in | form" (1+ i))))
    (reverse out-arg-list)))


;; (defmacro printf (fmt &rest args)
;;   (with-fmt (print fmt) args))

(defmacro if-test-val (test-fn val &rest forms)
  "run the first argument against val.  If val is true
   provide it to the forms as _it_"
  (declare (indent 2))

  `(let ((_it_ ,val))
     (if (,test-fn _it_) ,@forms)))

(defmacro if-not (test &rest forms)
  (declare (indent 2))

  `(if (not ,test) ,@forms))

(defun to-list (thing)
  "Convert thing to a list if it isnt already."
  (if (atom thing) (list thing) thing))

(defun to-array (thing)
  (etypecase thing
    (string (vector thing))
    (array thing)
    (list (vconcat thing))
    (t (vector thing))))

(defun vector-to-list (vec)
  (append vec nil))

(defun last-car (list)
  (car (last list)))

;; TODO(mls): Why not use subseq?
(defun take (n seq)
  "Take `n' from the start of `seq'.  Note that this
  is not implemented efficiently, so only use when n is small."
  (cl-loop for x below (min n (length seq))
           collect (elt seq x)))

(defun drop (n seq)
  "Drop the first `n' elements from `seq'.

  Note that this is not efficient, don't use
  it for large sequences."
  (cl-loop for x from n below (length seq)
           collect (elt seq x)))

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

   (let ((alist-args '())
         (rest-arg nil)
         (single-arg nil))

     (cl-labels
         ((make-arg-list (arg-alist)
             ;; Converts an alist of (argpos . gensym) pairs into the
             ;; argument list for the function.
             (let ((out-arg-list '()))
               ;; TODO(scheinholtz): Switch to loop, to get rid of the (1+ i)
               (dotimes (i (length arg-alist))
                 (let ((arg-pos (1+ i)))
                   (if-let (res (assoc (number-to-string arg-pos) arg-alist))
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
                   (if-let (arg-num (car (string-find "^%\\(.*\\)" (symbol-name elm))))
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
                         (t (if-let (old-sym (cdr (assoc arg-num alist-args)))
                                      (setf new-name old-sym)
                                      (setf alist-args (cl-acons arg-num new-name alist-args)))))
                        new-name)
                      elm))
               elm)))

       ;; Bindings for the state produced by the code processing functions.
       (let ((new-forms (code-walker #'perc->gensym in-forms))
             (arg-list (make-arg-list alist-args)))

         ;; validate arguments and handle single %
         (when single-arg
           (if (or rest-arg alist-args)
               (error "Cannot have %%& or %%1-%%n with %%"))
           (setf arg-list (list single-arg)))

         `(lambda ,(if rest-arg
                       (append arg-list `(&rest ,rest-arg))
                     arg-list)
            ,@(if (atom (car new-forms))
                  `(,new-forms)
                new-forms))))))

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
  (declare (indent defun))
  (let* ((destruct-list '())
         (arg-list (mapcar
                    (lambda (elm)
                      (if (atom elm)
                          elm
                        (let ((sym (gensym)))
                          (pushcons elm sym destruct-list)
                          sym)))
                    args)))
    `(lambda ,arg-list
       ;; Build nested destructuring binds.
       ,@(cl-reduce (lambda (forms bind-info)
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
  (cl-reduce (lambda (accum form)
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

  (cl-reduce (lambda (accum form)
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
  "Generate a list of `n' `elm's."
  (cl-loop for x below n collect elm))

(defun repeat-string (n str)
  (let ((out ""))
    (cl-loop for x below n do
             (setf out (concat out str)))
    out))

(cl-defun partition (n list &key (step n) (pad nil))
  "A grouping function.
  Split a list into sublists of length n.  A step value is optional.

Example:
 (partition 2 '(1 2 3 4 5) :step 1)
 -> ((1 2) (2 3) (3 4) (4 5) (5))"
  (loop for sublist on list by
        (lambda (elm) (drop step elm))
        collect (let ((chunk (take n sublist)))
                  (if pad
                      (let ((chunk-len (length chunk)))
                        (if (< chunk-len n)
                            (append chunk (repeat-elm (- n chunk-len) pad))
                          chunk))
                    chunk))))

(defun remove-if-not-regex (regex list &optional replace)
  (let ((res (cl-remove-if-not (| string-match regex %) list)))
    (if (string-has-value-p replace)
        (mapcar (| replace-regexp-in-string regex replace % t) res)
      res)))

(defun all-true (list)
  "Is this entire sequence true values?"
  ;; Note, since or is a macro, we can't do 'or
  (cl-reduce (| and %1 %2) list))

(defun any-true (list)
  "Are any members of this list true?"
  ;; Note, since or is a macro, we can't do 'or
  (cl-reduce (| or %1 %2) list))

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

(defun pprint (obj)
  (pp obj)
  nil)

(cl-defun find-thing-at-point (start-regex &key max-len end-regex (thing-name "thing"))
  "Use regexes to extract a piece of the buffer, and return it to a callback.

This should be used to write find-x-at-point functions.

`start-regex': used with re-search-backward to find the start of a thing.
`end-regex': optional re-search-forward to find the end of a thing, otherwise uses `start-regex'.
`max-len': optional arg to constrain the length.
"
  (save-excursion
    (let* ((start (1+ (save-excursion (re-search-backward start-regex))))
           (end (1- (save-excursion (re-search-forward (or end-regex start-regex)))))
           (len (- end start))
           (thing (if (< 0 len)
                       (buffer-substring-no-properties start end)
                     nil)))

      (if (and thing
               (or (not max-len)
                   (< len max-len)))
          thing
        (error "Unable to find valid %s.  Start: %d End: %d Len: %d" thing-name start end len)))))

(defun find-url-at-point ()
  (find-thing-at-point "[^A-Za-z0-9-._~:/?#@!$&'*+,;%]"))

(defun find-number-at-point ()
  (find-thing-at-point "[^0-9]"))

(cl-defun make-counter (&optional (start 0) (inc 1))
  "Create a stateful counter."
  (let ((amount start))
    (fn ()
        (cl-incf amount inc))))

(unless (fboundp 'curry)
  (defun curry (fn &rest orig-args)
    "Do a partial application of a function `FN'.

Example: (funcall (curry #' 1 2 3 4) 5 6))"
    (lambda (&rest rest-args)
      (apply fn (append orig-args rest-args)))))

(unless (fboundp 'curry*)
  (cl-defmacro curry* ((fn &rest args))
    "Use `curry' without modifying the function call."
    `(curry (function ,fn) ,@args)))

(unless (fboundp 'rcurry)
  (defun rcurry (fn &rest orig-args)
    "Do a partial application of a function `FN'.

Example: (funcall (rcurry #'- 10) 5)) -> -5"
    (lambda (&rest rest-args)
      (apply fn (append rest-args orig-args)))))

(unless (fboundp 'rcurry*)
  (cl-defmacro rcurry* ((fn &rest args))
    "Use `rcurry' without modifying the function call."
    `(rcurry (function ,fn) ,@args)))

(unless (fboundp 'compose)
  (defun compose (&rest fns)
    (let ((fns (reverse fns)))
      (lambda (&rest args)
        (cl-reduce (lambda (val f)
                     (funcal f val))
                   (rest fns)
                   :inital-value (apply (first fns) args))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; String functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO(scheinholtz): make a multi arg version
(defun string-split (sep-regex str)
  (split-string str sep-regex))

(defun string-nil-or-empty-p (s)
  (= 0 (length s)))

(defun string-has-value-p (s)
  (not (string-nil-or-empty-p s)))

(defun string-ends-with (suffix str)
  (string-match (concat (regexp-quote suffix) "$") str))

(defun string-starts-with (prefix str)
  (string-match (concat "^" (regexp-quote prefix)) str))

(defun string-or (&rest args)
  ;; Loop through the args, return the first not nil or empty
  ;; string
  (dolist (str args) (if-not (string-nil-or-empty-p str) (return str))))

(defun string-trim-chars (str front-group back-group)
  "Remove leading and trailing characters from a string."
  (replace-regexp-in-string (format "\\(^[%s]+\\|[%s]+$\\)" front-group back-group t) "" str))

(defun string-left-trim-regex (regex str)
  (replace-regexp-in-string (format "^%s" regex) "" str))

(defun string-right-trim-regex (regex str)
  (replace-regexp-in-string (format "%s$" regex) "" str))

;; (defun string-strip-non-ascii (s)
;;   (concat (cl-loop for c across s
;;                    if (aref printable-chars c)
;;                    collect c)))

(defmacro if-string (obj &rest forms)
  "Execute the true form if the string is length > 0"
  (declare (indent 2))
  `(if (string-has-value-p ,obj)
       ,@forms))

(defun replace-regex-region (regex replacement begin end)
  (save-excursion
    (goto-char begin)
    (while (re-search-forward regex end t)
      (replace-match replacement))))

(defun string-truncate (str max-len)
  (concat (cl-loop for s across str
                   for i below max-len
                   collect s)))

(cl-defun escape-string (str escape-chars &optional (escaper "\\"))
  "Escape a the string `str', that is put an `escape-char' in front of
each character in the string `chars'."
  (let ((escape-chars (concat escaper escape-chars)))
    (with-output-to-string
      (cl-loop for c across str
               when (cl-find c escape-chars)
                  do (princ escaper)
               do (princ (char-to-string c))))))

(defun quote-str (str)
  "Quote a string, escaping '\" and \\"
  (concat "\"" (escape-string str "\"\'\\") "\""))

(defun string-no-nil (value)
  "If `VALUE' is nil, return empty string.

This is to make working with some string processing code easier."
  (if value value ""))

(cl-defmacro string-when (test &body body)
  "A `when' function that returns \"\" on nil"
  (declare (indent defun))

  `(string-no-nil (when ,test
                    ,@body)))

(cl-defmacro string-when-let (test &body body)
  "A `when-let' function that returns \"\" on nil"
  (declare (indent defun))

  `(string-no-nil (when-let ,test
                    ,@body)))

(cl-defmacro string-when-let* (test &body body)
  "A `when-let' function that returns \"\" on nil"
  (declare (indent defun))

  `(string-no-nil (when-let* ,test
                    ,@body)))
;;
;; Not working just yet :p.
;;
(defun dequote-str (str)
  "Remove quotations from a string.

   Not sure that this is the best implementation, but
   it supports ' \" ` and \\ to escape"

  (let ((i 0)
        (out-str "")
        (quote-char nil)
        (in-quote nil))
    (while (< i (length str))
      (let ((c (aref str i)))
        (if in-quote
            (cond
              ((equal c quote-char) (setf in-quote nil))
              ((equal c ?\\) (progn
                               (cl-incf i)
                               (append-atom! out-str (aref str i))))
              (t (append-atom! out-str c)))
          (cond
            ((member c '(?\" ?\' ?\`))
             ;; record the quote character
             (progn
               (setf quote-char c)
               (setf in-quote t)))
            ;; pass it on like normal.
            (t (append-atom! out-str c)))))
      (cl-incf i))
    (concatenate 'string out-str)))

(defun string-case= (s1 s2)
  "Compare s1 and s2 ignoring case"
  (string= (downcase s1) (downcase s2)))

(defun find-longest-string-match (s list)
  (cl-first (sort
             (filter (fn (env-key) (string-starts-with env-key s)) list)
             (fn (e1 e2) (> (length e1) (length e2))))))

(defun string-find-named (regex str match-names)
  "Return an alist of matching captures.

  `regex': Regex with \((\)) captures
  `str': string to match
  `match-names': a list of strings or symbols in order of the groups in the regex.

  returns: nil on failed match
           an alist on success
"
  (when-let (result (string-find regex str))
    (cl-loop for name in match-names
             for data in result
             collect (cons name data))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbol functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun symbol-starts-with (prefix symb)
  (string-starts-with prefix (symbol-name symb)))

(defun symbol-matches (re symb)
  (string-match re (symbol-name symb)))

(defun symbol-equal-ignore-case (s1 s2)
  (cl-flet ((upcase-symbol (s)
              (upcase (symbol-name s))))
    (equal (upcase-symbol s1) (upcase-symbol s2))))

(defun symbol-rename (sym cb)
  "Create a new name based off the name of `sym'.

   The `cb' function takes the string name of the symbol
   and should return a string the caller wants converted into
   a new symbol.

   Example: (symbol-rename 'abc (| (concat % \"def\"))) -> abcdef
   "
  (intern (funcall cb (symbol-name sym))))

(defun string-to-keyword (str)
  (intern (concat ":" str)))

(defmacro string-cl-case (expr &rest clauses)
  "A string version of `cl-case'."
  (declare (indent 1) (debug (form &rest (sexp body))))
  (with-gensyms (gexpr-result)
    `(let ((,gexpr-result ,expr))
       (cond
        ,@(mapcar (fn ((str &rest body))
                    (if (equal str 't)
                        `(t ,@body)
                      `((equal ,gexpr-result ,str) ,@body)))
                  clauses)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defmacro with-fmt ((fun str) &rest args)
  (declare (indent defun))
  `(,fun (format ,str ,@args)))

;; TODO(scheinholtz): Use the async I/O for this.
(cl-defun doit-shell (doit-str silent fmt &rest args)
  (let ((cmd (apply #'format fmt args))
        (doit (string= "yes" doit-str)))
    (when (or (not doit) (not silent))
      (switch-to-buffer "*shell-cmd-buf*")
      (-> cmd (concat "\n") insert)
    (if doit (shell-command cmd)))))

(defun cmd-to-shell-string (cmd)
  "Convert a list of raw strings into something you can
  pass to bash -c."

  ;; NOTE! I originally did this:
  ;; (combine-and-quote-strings (mapcar #'shell-quote-argument cmd)
  ;;
  ;; But that is wrong, it's essentially doing a double quote,
  ;; I either need to do something like:
  ;; 'always quote' with ', or just do a regular string join
  ;; on the shell-quote-argument strings.
  ;;
  (string-join (mapcar #'shell-quote-argument cmd) " "))

;;
;; Design Notes:
;;
;; ideal interface:
;; (run-str "ls -l") -> "string of contents"
;; (run "ls" "-l" :stdout 'string :stderr "/dev/null")
;;
;; I may also want to add an 'environment' input to the function.
;; for stuff like ecr access.
;;
;; Ponder how to do async.
;;  Issues:
;;   - start-process vs call-proc command differences.
;;   - should I just make a new command?
;;
;;  if I use start-process I should, set (process-connection-type nil)
;;
;;  It seems like you get a handle back, that you can use
;;  to see what happened (it also creates a communication buffer.)
;;
;; TODO(mls): is it useful to have an :input-string flag,
;;            to save having to create the file first?
;;
(cl-defun do-cmd (cmd &key input stdout stderr throw ssh sudo)
  ;; Add an async function
  "Be a main entry point for running shell commands.

  Wrapper around emacs's external shell command function (call-process),
  hopefully with a nicer interface.

  It executes the command and returns an alist with the results
  or any output if requested.

  The arguments are:
   1. `cmd': a list with the program to run and its arguments.
             The arguments won't be interpreted by the shell.

   2. `input': The file to hand to the input process.  nil is the same as /dev/null

   3. `stdout': What to do with the stdout from the subprocess.
               'current-buffer: Dump into the current buffer.
               'string:         Return it as a string in the result alist.
               <buffer object>
               nil
               (:buffer <buffer-name>)
               (:file <file-path> 'append) ; append is optional.

   4. `stderr': The same as stdout with one additional option.
                NOTE: this is on by default to make error messages better.
                'stdout: combine stdout and stderr.
   5. `throw': Raise an error on failure instead of returning the alist.

  output: (list
           (:code . <int>) ; Did it succeed or fail
           (:stdout . \"string\")  ; if any
           (:stderr . \"string\")) ; if any
  "
  ;; TODO: Consider switching to with-tempdir, and using
  ;; it to cleanup the stderr file, and generate/cleanup the input file.
  (let* ((program (cl-first cmd))
         (arguments (cdr cmd))
         (stdout-buffer (when (equal stdout 'string)
                          (generate-new-buffer "*do-cmd-stdout*")))
         ;; We always save stderr.
         (stderr-file (make-temp-file "do-cmd-stderr"))
         (stdout (if stdout-buffer t stdout))
         (stderr (if stderr-file stderr-file stderr))

         ;; Add a section to remap stderr/stdout
         (stderr (cl-case stderr
                   (stdout t)
                   (current-buffer t)
                   (otherwise stderr)))
         (stdout (cl-case stdout
                   (current-buffer t)
                   (otherwise stdout)))
         (resp)
         (stderr-string))

    ;; Deal with ssh commands
    (when ssh
        (setf program "ssh")
        (setf arguments (cons ssh cmd)))

    (cl-flet ((my-call-process ()
                (message "cmd: %s %s" program (cmd-to-shell-string arguments))
                (setf resp (apply #'call-process program input (list stdout stderr) nil arguments))))
      ;; Use unwind protect here, so we always cleanup the stdout buffer.
      (unwind-protect
          (progn
            ;; Do the execution.
            (if stdout-buffer
                (with-current-buffer stdout-buffer
                  (my-call-process))
              (my-call-process))

            ;; Handle stderr output.
            (when (and stderr-file (file-exists-p stderr-file))
              (setq stderr-string (slurp stderr-file))
              (delete-file stderr-file))

            (let ((stdout-string (if stdout-buffer
                                   (with-current-buffer stdout-buffer
                                     (buffer->string))
                                   "")))

              ;; Check for any errors, do we need to throw?
              (when (and (not (equal resp 0))
                         throw)
                (error "Command %s %s failed, code: %s, msg: \'%s\'"
                       program arguments resp
                       (if-let (error-str (or (string-trim (s-truncate 1024 stderr-string))
                                              (string-trim (s-truncate 1024 stdout-string))))
                           error-str
                         "")))

              ;; return the result
              (let ((output '()))
                (push (cons :code resp) output)
                (pushcons :stdout stdout-string output)
                (pushcons :stderr stderr-string output)
                output)))
        ;; Always cleanup the stdout buffer we created.
        ;; all cleanup code should go there.
        (when stdout-buffer
          (let ((kill-buffer-query-functions nil))
            (kill-buffer stdout-buffer)))))))

(defun do-cmd-succeeded-p (results)
  (= 0 (assoc1 :code results)))

(defun run (&rest cmd-parts)
  "Execute a shell command given an argument list.  See `do-cmd'
   for return value.

   Example:   (run \"hdiutil\" \"attach\" path)"
  (do-cmd cmd-parts :throw t))

(defun run-bg (&rest cmd-parts)
  "Run a command a `run' would, but do it in the background.
Don't expect any output."
  (run "bash" "-c" (concat (cmd-to-shell-string cmd-parts) " &")))

(defun run-to-str (&rest cmd-parts)
  "Run a command using `do-cmd' and return the result as a string.

   It will throw an error if the command fails."
  (assoc1 :stdout (do-cmd cmd-parts :stdout 'string :throw t)))

(defun run-logged (&rest cmd-parts)
  "Run a command like `run' but dump the output to *messages*"
  (let ((res (do-cmd cmd-parts :stdout 'string :stderr 'string :throw t)))
    (message "stdout: %s" (assoc1 :stdout res))
    (message "stderr: %s" (assoc1 :stderr res))
    res))

;; TODO(mls): maybe just make this a do-cmd option.
(defun run-to-json (&rest cmd-parts)
  (json-read-from-string (apply #'run-to-str cmd-parts)))

(defun run-to-str-ssh (host &rest cmd-parts)
  "Run a command with ssh on the remote host."
  (assoc1 :stdout (do-cmd cmd-parts :stdout 'string :throw t :ssh host)))

(defun run-is-true (&rest cmd-parts)
  (do-cmd-succeeded-p (do-cmd cmd-parts)))

(defun which (executable)
  (let ((res (do-cmd (list "which" executable) :stdout 'string)))
    (when (do-cmd-succeeded-p res)
        (string-trim (assoc1 ':stdout res)))))

;;
;; questions:
;; 1. What code can be shared with do-cmd?
;; 2. How closely should I match the behavior of do-cmd?
;;    - For now keep it simple, and just use the popular features of do-cmd.
;;
;; 3. Should I eventually define do-cmd as a wrapper around do-cmd-async?  Is
;;    that too inefficient somehow?
;; 4. I should provide more options to stdout and stderr, so they can use
;;    buffers in addition to strings.
;;
;; FYI, I learned that timers aren't really reliable.  I'm going to
;; try set-process-sentinel.
;;
(let ((do-cmd-async-counter (make-counter)))
  (defun next-do-cmd-id ()
    "Return the next command id `do-cmd-async' should use for logging."
    (funcall do-cmd-async-counter)))

(cl-defun do-cmd-async (cmd &key callback-fn input input-str stdout stderr throw cwd)
  "Use Emacs's make-process function to run a command in async fashion.

The goal is to do work in the background without locking Emacs until
the command completes.

The arguments are:
1. `cmd': a list with the program to run and its arguments.
          The arguments won't be interpreted by the shell.
2. `callback-fn': A function that takes one argument (the response object
                  described below).
4. `input': A path to a file to send as input/stdin to the command.
5. `input-str': A string to send to the input process as stdin.
5. `stdout': What to do with the stdout from the subprocess.
             'string:         Return it as a string in the result alist.
6. `stderr': Include stderr in the response object.
7. `throw': Raise an error on failure.
            Currently won't call the callback fn.
8. `cwd': Set the default-directory for running the command

output: (list
         (:code . <int>) ; Did it succeed or fail
         (:stdout . \"string\")  ; if any
         (:stderr . \"string\")) ; if any

output is passed to the callback-fn."

  (let* ((program (cl-first cmd))
         (args (rest cmd)))
    ;;
    ;; make a buffer.
    ;;
    ;; save local variables:
    ;; 1. stdout options
    ;; 2. stderr options
    ;; 3. throw
    ;;

    (let ((stdout-buffer (generate-new-buffer (format "<cmd-buffer-%s-output-%s" program (uuidgen-4))))
          (stderr-buffer (generate-new-buffer (format "<cmd-stderr-buffer-%s-output-%s" program (uuidgen-4))))
          (do-cmd-id (next-do-cmd-id)))

      (with-current-buffer stdout-buffer
        (set (make-local-variable 'args) args)
        (set (make-local-variable 'stdout) stdout)
        (set (make-local-variable 'stderr) stderr)
        (set (make-local-variable 'stderr-buffer) stderr-buffer)
        (set (make-local-variable 'throw) throw)
        (set (make-local-variable 'program) program)
        (set (make-local-variable 'callback-fn) callback-fn)
        (set (make-local-variable 'do-cmd-id) do-cmd-id)

        ;;
        ;; I don't know that I need to set both buffer local and the global default diretory
        ;; I'm possibly being overly paranoid.
        ;;
        (let ((default-directory (or cwd default-directory)))
          (setq-local default-directory default-directory)

          (message "do-cmd-async[%d]: %s %s in %s -> " do-cmd-id program
                   (cmd-to-shell-string args) default-directory)

          (let ((proc (make-process :name program
                                    :buffer stdout-buffer
                                    :stderr stderr-buffer
                                    :command (cons program args)
                                    :connection-type 'pipe
                                    :sentinel #'ignore)))
            (when (or input input-str)
              (process-send-string proc (or input-str (slurp input)))
              (process-send-eof proc))

            (set-process-sentinel proc #'do-cmd-async-finish)
            proc))))))

(cl-defun do-cmd-async-finish (proc change-desc)
  (let ((got-error))
    (unwind-protect
        (with-current-buffer (process-buffer proc)
          (let* ((code (process-exit-status proc))
                 (output))

            (setf got-error (and throw (not (equal 0 code))))

            (message "Stdout buffer: %s  stderr-buffer: %s" (process-buffer proc) stderr-buffer)
            (let ((process-buffer (process-buffer proc))
                  (stdout-string (if (equal stdout 'string)
                                     (or (buffer->string) "")
                                   ""))
                  (stderr-string  (with-current-buffer stderr-buffer
                                    (buffer->string))))
            (when got-error
              (error "-> FAILED! Async Command: %s %s.  Check buffers: %s and %s.  Error msg: %s"
                     program (cmd-to-shell-string args)
                     process-buffer stderr-buffer
                     (if-let (error-str (or (string-trim (s-truncate 1024 stderr-string))
                                            (string-trim (s-truncate 1024 stdout-string))))
                           error-str
                         "")))

            (pushcons :stdout stdout-string output)
            (pushcons :stderr (if (eql stderr 'string)
                                  stderr-string
                                "")
                      output)
            (pushcons :code code output)

            (message "do-cmd-async[%s]: -> finished :(%s): %s %s" do-cmd-id code program (cmd-to-shell-string args))
            (when callback-fn
              (funcall callback-fn output)))))
      (let ((stderr-buffer (with-current-buffer (process-buffer proc)
                             stderr-buffer))
            ;; Setting this to nil eliminates the query.
            (kill-buffer-query-functions nil))
        (unless got-error
          ;; Don't delete the buffers if we got an error... maybe make this an option?
          (when-let (pb (process-buffer proc))
            (kill-buffer pb))
          (when stderr-buffer
            (kill-buffer stderr-buffer)))))))

(cl-defun run-async (cmd &key cwd cb)
  (do-cmd-async cmd
                :callback-fn (lambda (result)
                               (message "Result from %s: %s" cmd result)
                               (assert (do-cmd-succeeded-p result))
                               (when cb
                                 (funcall cb result)))
                :stdout 'string
                :stderr 'string
                :cwd cwd))

(defun run-to-str-async (callback-fn &rest cmd-parts)
  (assert cmd-parts)
  (do-cmd-async cmd-parts
                :stdout 'string
                :throw t
                :callback-fn (lambda (resp)
                               (funcall callback-fn (assoc1 :stdout resp)))))

(defun run-to-json-async (callback-fn &rest cmd-parts)
  (apply #'run-to-str-async
         (lambda (s)
           (funcall callback-fn (json-read-from-string s)))
         cmd-parts))

(cl-defun do-cmd-async-to-buffer (buffer-name
                                  &key cmd json generate-new-buffer)
  (do-cmd-async cmd
                :throw t
                :stdout 'string
                :callback-fn (lambda (resp)
                               (let ((buffer-name (if generate-new-buffer
                                                      (generate-new-buffer-name buffer-name)
                                                    buffer-name)))
                                 (assert (do-cmd-succeeded-p resp))

                                 (let ((output (assoc1 :stdout resp)))
                                 (if json
                                     (with-overwrite-buffer-pp buffer-name
                                       (json-read-from-string output))
                                   (with-overwrite-buffer buffer-name
                                     (insertf "%s" output))))))))

(cl-defun wait-for-procs (procs)
  "Wait for a list of emacs processes to finish."

  (cl-loop for proc in procs
           do (cl-loop
               (if (not (process-live-p proc))
                   (cl-return)
                 (progn
                   (message "Sleep for procs")
                   (sleep-for 1))))))

(defun parallel-commands (cmds)
  "Run a list of commands and callback function cons cells in parallel.

So pass a list like:
 '(((\"ls\" \"-l\" \"/tmp\") . #'handler)
   ((\"ls\" \"-l\" \"/tmp\") . #'handler))
"
   (wait-for-procs (cl-loop for (cmd . cb-fn) in cmds
                            collect (do-cmd-async cmd
                                                  :throw t
                                                  :stdout 'string
                                                  :stderr 'string
                                                  :callback-fn cb-fn))))

;;;
;;; Some functions for dealing with arguments
;;;

(defun make-argument-builder ()
  ;;
  ;; (:opt name value)
  ;; (:arg value)
  ;; (:flag <name> value)
  ;;
  ;; (:args)
  ;;
  (let ((cli-args))
    (lambda (cmd &rest options)
      (cl-flet ((extend-args (&rest new-args)
                  (setf cli-args (concatenate 'list cli-args new-args))))
        (cl-ecase cmd
          (:opt (destructuring-bind (name value) options
                  (when value
                    (extend-args name value))))
          (:arg (destructuring-bind (value) options
                    (when value
                      (extend-args value))))
          (:flag (destructuring-bind (name value) options
                   (when value
                     (extend-args name))))
          (:cli-args cli-args))))))


;; TODO(scheinholtz): Unify buffer sections.
(defun string->list (str &optional regex)
  (mapcar #'string-trim (split-string str (or regex "\n") t)))

(defun tail (n cmd-result)
  "Get the last line of a command result"
  (let* ((lines (string->list cmd-result))
         (len (length lines)))
    (cl-loop for line in lines
             for x from 0
             if (>= x (- len n))
             collect line)))

(defun buffer->list ()
  "Convert the current buffer into a list."
  (string->list (buffer->string)))

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

(defun buffer->string ()
  "Get the entire content of the current buffer as a string.
Use this likely in leu of `buffer-string'."
  (buffer-substring-no-properties (point-min) (point-max)))

;;TODO(scheinholtz)
;;
;; The concept of "to" something may be an idiom
;; I can create.
(defun to-buffer (name str)
  (let ((new-buffer (-> name generate-new-buffer set-buffer)))
    (insert str)
    new-buffer))

(defun to-buffer-switch (name str)
  (switch-to-buffer (to-buffer name str)))

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

;; TODO(mike): Does it make sense to have these as macros if
;; I just switch to the new buffer?
;;
;; Is there a smarter way to do this?  The with- macro sort
;; of implies you'll do stuff after the macro is finished.
;;
;; XXX: I think I don't need this... I think
;; I can either use set-buffer or with-current-buffer.
;;
(defmacro with-new-buffer (name-prefix &rest body)
  (declare (indent defun))
  (let ((old-buffer (gensym))
        (new-buffer (gensym)))

    `(let* ((,old-buffer (current-buffer))
            (,new-buffer (generate-new-buffer ,name-prefix)))
       (switch-to-buffer ,new-buffer)
       (unwind-protect
           (progn
             ,@body)
         (switch-to-buffer ,old-buffer)))))

(defmacro with-overwrite-buffer (name &rest body)
  (declare (indent defun))
  (with-gensyms (buffer-name)
    `(let ((,buffer-name ,name))
       (switch-to-buffer ,buffer-name)
       (clear-buffer (current-buffer))
       ,@body
       (beginning-of-buffer)
       (current-buffer))))

(defmacro with-overwrite-buffer-pp (name &rest body)
  (declare (indent defun))
  `(with-overwrite-buffer ,name
     (lisp-mode)
     (insert (pp-to-string
              (progn
                ,@body)))))

;; (defun shell-open-with-command (dir cmd &optional name)
;;   "Open a new shell buffer and run a command in it."
;;   (pushd dir
;;     (with-current-buffer (shell (generate-new-buffer (or name (make-shell-buffer-name dir))))
;;       (insertf cmd)
;;       (comint-send-input nil t))))

;; Can this function be simplified?
(defmacro with-shell-buffer (dir name &rest body)
  (declare (indent defun))
  (with-gensyms (buffer buffer-name)
    `(let ((,buffer-name ,name))
       (if-let (,buffer (get-buffer ,buffer-name))
           (switch-to-buffer ,buffer)
         (progn
           (switch-to-buffer (shell-open-dir ,dir))
           (rename-buffer ,buffer-name)
           ,@body))
       ,buffer-name)))

(defmacro with-venv-buffer (dir name &rest body)
  (declare (indent defun))
  (with-gensyms (buffer buffer-name)
    `(let ((,buffer-name ,name))
       (if-let (,buffer (get-buffer ,buffer-name))
           (switch-to-buffer ,buffer)
         (progn
           (switch-to-buffer (open-shell-dir-venv ,dir))
           (rename-buffer ,buffer-name)
           ,@body))
       ,buffer-name)))

;; How do I make this switch to the current file directory?
(defun shell-open-dir (dir)
  "Create a shell in the given directory"

  (with-open-dir dir (lambda (name)
                       (shell name))))

(defun shell-dir ()
  "Open a shell in the current default directory"
  (interactive)
  (shell-open-dir default-directory))

(defun run-shell-command (cmd &rest args)
  ;; Trying to exit after running the command.
  (insertf "%s %s; exit" cmd (string-join (mapcar #'shell-quote-argument args) " "))
  (comint-send-input nil t))

(defun shell-open-run-command (dir cmd &optional name)
  "Open a new shell buffer and run a command in it."
  (with-shell-buffer dir (generate-new-buffer-name (or name (make-shell-buffer-name dir)))
    (apply #'run-shell-command (car cmd) (cdr cmd))))

(defun shell-open-with-command (dir cmd &optional name)
  (with-shell-buffer dir (generate-new-buffer-name (or name (make-shell-buffer-name dir)))
    (insertf (string-join (mapcar #'shell-quote-argument cmd) " "))))

(defun eshell-dir ()
  (interactive)
  (with-open-dir (lambda (name)
                   (eshell name)))
  (shell-open-dir default-directory))

(defmacro insertf (fmt &rest args)
  `(with-fmt (insert ,fmt)
     ,@args))

(defmacro insertf-send (fmt &rest args)
  `(progn
     (insertf ,fmt ,@args)
     (comint-send-input nil t)))

(defun get-current-line ()
  "Return the line under the cursor, with properties."
  (string-trim-right (thing-at-point 'line)))

(defun dedup (input)
  "Remove duplicates from the input sequence"
  (let ((seen (make-hash-table))
        (out '()))
    (dolist (elm input)
      (unless (gethash elm seen)
        (puthash elm t seen)
        (setf out (cons elm out))))
    (nreverse out)))

(defun duplicate-line ()
  (interactive)
  (save-excursion
    (end-of-line)
    (open-line 1))
  (beginning-of-line)
  (let ((line (get-current-line)))
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
;;(defun list->info-tuple (list)
;;  (loop )

;; Stolen from http://www.thekidder.com/2008/10/21/emacs-reload-file/
(defun reload-file ()
  (interactive)
  (let ((curr-scroll (window-vscroll)))
    (find-file (buffer-name))
    (set-window-vscroll nil curr-scroll)
    (message "Reloaded file")))

(defmacro append! (l &rest values)
  "NOTE! This only works with a symbol, not a complex type."
  (assert (symbolp l))
  `(setf ,l (append ,l ,@values)))

(defmacro append-atom! (l &rest values)
  "NOTE! This only works with a symbol, not a complex type."
  (assert (symbolp l))
  `(setf ,l (append ,l ,@(mapcar (fn (a) `(list ,a)) values))))

(defmacro append-cons! (l key value)
  "NOTE! This only works with a symbol, not a complex type."
  `(append-atom! ,l (cons ,key ,value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Time utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun usec->time (usec)
  (seconds-to-time (/ usec 1000000)))
'
;; XXX Should also make this run in the buffer (take the last N digits until someting not a number.
(defun usec->date (usec)
  (interactive "dusec: ")
  (format-time-string "%A, %B %e, %Y %r" (usec->time usec)))

;; These are what you want.
(defun current-time-msec ()
  (interactive)
  (-> (current-time) time-to-seconds (* 1000)))

(defun msec->time (msec)
  (seconds-to-time (/ msec 1000)))

(defun msec->date (usec)
  (interactive "dusec: ")
  (-> usec msec->time time->date))

;; XXX Define the conversion macros I have from clojure.
(defun sec->usec (sec)
  (* sec 1000000))

(defun current-time->usec ()
  (-> (current-time) time-to-seconds  sec->usec))

(cl-defun conv-time (time &key (seconds 0) (minutes 0) (hours 0) (days 0) (months 0) (years 0))
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
  (if-let (day-of-week (if (numberp day-of-week)
                             day-of-week
                         (assoc1 day-of-week week-days)))
    (do ((i 1 (+ i 1)))
        ((>= i 8))
      (let ((before (conv-time time :days (- i))))
        ;; The sixth spot is the day of the week.
        (if (= day-of-week (nth 6 (decode-time before)))
            (return before))))))

(defun time->date (time)
  ;; TODO(scheinholtz): there is probably a good letter for this
  ;; instead of listing it out explicitly
  (format-time-string "%A, %B %e, %Y %r" time))

(defun time-current-minus-days (days)
  (days-to-time (- (time-to-number-of-days (current-time)) days)))

(defun time-split (time)
  (mapcar* #'cons '(:second :minute :hour :day :month :year :dow :dst :utcoff) (decode-time time)))

(defun current-time-split ()
  (time-split (current-time)))

(defun current-time-dow ()
  (assoc1 :dow (current-time-split)))

(defun current-day-of-week ()
  (if-let (rec (rassoc (current-time-dow) week-days))
      (car rec)
    (error "Unable to find day of week")))

;; Maybe don't split the time, just use format-time-string
(defun other-day-of-this-week (day)
  (time-split (time-current-minus-days (- (current-time-dow) day))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File/Path utils.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun string-remove (pattern str)
  (replace-regexp-in-string pattern "" str))

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
  (split-string path directory-sep t))

(defun path-join (&rest args)
  "Join a set of path parts together, removing any duplicate slashes (OSX/Unix only)."
  (cl-flet ((needs-slash (val check-for-slash)
               (when (and val (funcall check-for-slash directory-sep val))
                 directory-sep)))
    (let ((leading-slash (needs-slash (cl-first args) #'string-starts-with))
          (trailing-slash (needs-slash (last-car args) #'string-ends-with)))
      (string-join (list leading-slash
                         (string-join (mapcar (| string-trim-chars %
                                                 directory-sep
                                                 directory-sep)
                                              args)
                                      directory-sep)
                         trailing-slash)))))

;; The opposite of basename is file-name-directory
(defalias 'basename 'file-name-nondirectory)

(defun touch (path)
  "Create an empty file at the given `path'"
  (run "touch" path))

(defun ensure-makedirs (dir)
  (unless (file-exists-p dir)
    (message "Creating directory: %s" dir)
    (make-directory dir t)))

(defun recreate-directory (path)
  (when (file-exists-p path)
    (delete-directory path t))
  (make-directory path t))

(defun is-real-file (path)
  (let ((name (basename path)))
    (not (or (string= "." name)
             (string= ".." name)))))

;; find-lisp makes this uneeded.. .keeping for poseterity.
(cl-defun traverse-directories (cb_fn start_dir &key (file_match nil) (max_depth 500))
  (mapc (lambda (file)
          (if (or (not file_match) (string-match file_match (file-name-nondirectory file)))
              (funcall cb_fn file))
          (if (and (>= max_depth 0) (file-directory-p file))
              (traverse-directories cb_fn file :file_match file_match :max_depth (1- max_depth))))
        (cl-remove-if-not #'is-real-file  (directory-files start_dir t))))

(defun directory-last-dirname (path)
  "Return the name of the last directory in the path.

Example:
  (directory-last-dirname \"/a/b/c.txt\") -> \"b\""
  (-> path file-name-parent-directory f-split last-car))

(cl-defun list-directory-entries (dir &key full ignore match nosort files-only dirs-only filter)
  "Return only the files in `dir' with some options.

   There are lots of Emacs functions which do similar things,
   I'm trying to make this the most useful.

   There is `directory-files', `list-directory',
   `directory-files-recursively', `file-name-all-completions' with "".

   The options are:
     `full': Return the full path name for each entry.
     `ignore': An emacs regex of files to ignore.
     `match': An emacs regex of files to keep.  Like `directory-files' match.
     `nosort': Don't sort the file names.  Like `directory-files' sort."

  (remove-if (fn (path)
               (let* ((name (basename path))
                      (full-path (expand-file-name (path-join dir name))))
                 (or (string= "." name)
                     (string= ".." name)
                     (and files-only (file-directory-p full-path))
                     (and dirs-only (not (file-directory-p full-path)))
                     (and filter (not (funcall filter name)))
                     (and ignore
                          (string-match ignore name)))))
             (directory-files dir full match nosort)))

(defun current-last-dirname ()
  (directory-last-dirname default-directory))

(defun file-list-parents (start-dir)
  "Return a list of all of the parent directories of `start-dir'.
Note that this includes start-dir itself."
  ;; Make sure we're only dealing with a directory, or
  ;; this wont' work.
  (let ((dir (file-name-directory start-dir)))
    (cl-loop for i below 100
             collect dir
             until (or (equal dir "/")
                       ;; OSX is weird.
                       (equal dir "/private"))
             do (setf dir (file-truename (path-join dir ".."))))))

(defun directory-file-under-dir (dir file)
  "Is the `file' path underneath the directory `dir'?"
  (string-starts-with (file-truename dir) (file-truename file)))

(defun slurp (path)
  "Read the contents of the file specified by `path' into a string.

  Return that string to the caller, or throw an error on failure."

  (with-temp-buffer
    (insert-file-contents path)
    (buffer->string)))

(defun barf (str path &optional append)
  "Write a file to disk."
  (with-temp-buffer
    (insert str)
    (write-region nil nil path append)))

(defun barf-binary (data path)
  "Write a binary blob to disk."
  (let ((coding-system-for-write 'no-conversion))
    (with-temp-buffer
      (toggle-enable-multibyte-characters -1)
      (set-buffer-file-coding-system 'raw-text)
      (insert data)
      (write-region nil nil path))))

(defun overwrite (str path)
  (assert (file-regular-p path))
  (let ((tmp-path (concat path ".tmp")))
    (unwind-protect
        (with-temp-file tmp-path
          (insert str))
      (when (file-exists-p tmp-path)
        (delete-file tmp-path)))))

(defun list-to-alist (list map)
  (cl-loop for l in list
           for key in map
           collect (cons key l)))

(defun get-file-info (path)
  "Return the file system information for a given path."

  (unless (file-exists-p path)
    (error "File %s does not exist" path))

  (list-to-alist
   (file-attributes path)
   '(type
     num-links
     uid
     gid
     last-access-time
     last-modification-time
     last-status-change-time
     size
     mode
     ignore
     inode-number
     device-number)))

(defun get-file-modification-time (path)
  (assoc1 'last-modification-time (get-file-info path)))

(defun get-file-size (path)
  "Return the size of the file specified by `path' in bytes."
  (assoc1 'size (get-file-info path)))

(defun file-has-size-p (path)
  (when (file-exists-p path)
    (> (get-file-size path) 0)))

(cl-defun du (dir &key (max-depth 2) callback-fn)
  "Call `du' and use a callback.

The `callback' function should take one argument,
which is the same as the result alist returned
by `do-cmd'
"
  (do-cmd-async (list "bash" "-c" (format "du -h -d %s \'%s\' | sort -h" max-depth (expand-file-name dir)))
                :stdout 'string
                :throw t
                :callback-fn callback-fn))

(cl-defun du-output (dir max-depth)
  (interactive (list (read-string "dir: " "~")
                     (completing-read "max-depth: " nil nil nil "2")))
  (du dir
      :max-depth max-depth
      :callback-fn (lambda (result)
                     (with-overwrite-buffer-pp (format "+du-for-%s-du+" dir)
                       (csv-split-text (assoc1 :stdout result)
                                       :split-regex "[\t ]+"
                                       :has-header-line nil
                                       :field-names '("size" "path"))))))

(defun assert-program-exists (name &optional error-message)
  "Assert that a program specified by `name' is in the path.

   If it is, do nothing, if not, throw an error with the relevant
   `error-message'"

  (assert (which name) nil (or error-message (format "Program %s is not in the PATH." name))))

;;(defun find-and-grep )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: switch to using the CSV code?
;; TODO: also make a custom grouper, that can handle
;;       multiple matches, etc.

(defun list-all-processes ()
  "Return a list of alists for all running processes.  Each line
from ps auxwww becomes an alist with an entry for each field
supplied by the command."
  (let* ((lines (mapcar (fn (line)
                          (split-string line "[ \t]+" t))
                        (string->list (run-to-str "ps" "auxwww"))))
         (labels (mapcar (| intern (downcase %)) (cl-first lines)))
         (processes (rest lines)))
    (mapcar (fn (proc-info)
              (mapcar* #'cons labels proc-info))
            processes)))

(defun list-open-tcp-connections ()
  (csv-split-text (run-to-str "lsof" "-i" "tcp") :split-regex "[ \t][ \t]+"))

(defun list-open-files-buffer ()
  (interactive)
  (with-overwrite-buffer "+list-open-files-lsof+"
    (shell (current-buffer))
    (insertf "lsof -i tcp")))

(defun process-is-running-regex-p (proc-name-regex)
  "Check all running processes agaist `PROC-NAME-REGEX'."
  (cl-loop for proc-info in (list-all-processes)
           if (string-match proc-name-regex (assoc1 'command proc-info))
           return t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer Utils.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun list->buffer (list buffer-name)
  (set-buffer (generate-new-buffer buffer-name))
  (insert (string-join list)))

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
        (string-join (butlast split-name 1) ".")
      (car split-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode / outline-mode utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mls-org-get-section-body-at-point ()
  (save-excursion
    (outline-back-to-heading t)
    (org-end-of-meta-data)

    ;; Skip any log lines.
    (while (org-in-item-p)
      (message "point is %s" (point))
      (next-line))

    (buffer-substring-no-properties (point)
                                    (progn (outline-next-preface) (point)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Network Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Don't overlap with what's in `net-utils'
;;
;; netstat
;; also use lsof
;;

(defun osx-list-open-files ()
  "Find out which files, sockets etc are open.
Returns a list of alists."
  (cl-loop for line in (string->list (run-to-str "lsof" "-Pnln" "+M" "-i" "-cmd"))
           for lineno from 1
           when (> lineno 1)
              ;; This is like zip
              collect (mapcar* #'cons
                               '(:command :pid :user :fd :type :device :size/off :node :name :action)
                               (split-string line))))
(defun osx-list-ports-in-use ()
  (cl-flet ((name-to-port (file)
              (when-let (action (assoc-get :action file))
                (when (equal "(LISTEN)" action)
                  (when-let (port (car (string-find "^\\*:\\([0-9]+\\)$" (assoc1 :name file))))
                    (string-to-number port))))))

    (cl-loop for file in (osx-list-open-files)
             when (name-to-port file)
               collect (cons (name-to-port file) file))))

(defun osx-port-in-use (port)
  (assoc-get port (osx-list-ports-in-use)))

(defun ip-addr-to-list (ip-addr)
  (mapcar #'string-to-number (split-string ip-addr "\\.")))

(defun osx-list-hal-plugins ()
  (cl-loop for dir in (list "/Library/Audio/Plug-Ins/HAL/"
                            (expand-file-name "~/Library/Audio/Plug-Ins")
                            "/System/Library/Audio/Plug-Ins/HAL/")
           when (file-exists-p dir)
           append (list-directory-entries dir :full t)))

(defun osx-list-bluetooth-info ()
  (run-to-str "system_profiler" "SPBluetoothDataType"))

(defconst +osx-coreaudio-program+ "com.apple.audio.coreaudiod")

(defun osx-launchctl (cmd program)
  (let ((default-directory "/sudo::"))
    (run "launchctl" (symbol-name cmd) program)))

(defun osx-stop-coreaudio ()
  (osx-launchctl 'stop +osx-coreaudio-program+))


;; sudo launchctl stop com.apple.audio.coreaudiod


;;
;; Work on these later.
;;
;; (defun ipal-to-decimal (ipal)
;;   (loop for x ))

;; (defun ipal-to-hexidecimal (ipal))

;; (defun cidr-addr-info-32bit (ip-addr)
;;   "Return some info about a CIDR address."

;;   (let ((total-addr-bits 32))
;;     (destructuring-bind (prefix netmask-bits)
;;         (string-find "\\([\\.0-9-]+\\)/\\([0-9]+\\)" ip-addr)
;;       (let ((host-bits (- total-addr-bits netmask-bits)))
;;         ;; Break a 32 bit number into an ip address



;;         `((:cidr-range . ,ip-addr)
;;           (:netmask . )
;;           (:wildcard-bits . )
;;           (:first-ip . )
;;           (:last-ip . )
;;           (:total-hosts . ,(expr 2 host-bits)))
;;       ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Google Search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun google-search-terms (term-string)
  (let ((google-cookie-jar "~/.google-cookies.jar")
        (google-url "https://www.google.com/")
        (user-agent "Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 6.0)"))

    ;; (web-request google-url
    ;;              :user-agent user-agent
    ;;              :cookie-jar google-cookie-jar)
    ;; (sleep-for 2)
    (let ((resp (web-request (path-join google-url "search") :params `((q . ,term-string)
                                                           (hl . "en")
                                                           (btnG . "Google Search"))
                 :user-agent user-agent
                 :cookie-jar google-cookie-jar
                 :throw t)))

      ;; Need to work on parsing the results.
      ;; use something like xpath/beautifulsoup, or something to deal
      ;; with the sexps.
      (assoc1 :html resp)
      )))

(defun google-search-at-point ())
(defun google-search-region ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insert-interactive ()
  "Generate an (interactive) declaration for a function."
  (interactive)
  (let ((pos (point)))
    (re-search-backward "(defun +[^ ]+ +(\\(.*?\\))")
    (if-let (arg-str (match-string-no-properties 1))
       (progn
         (printf "arg-str: |%s|\n" arg-str)
         (goto-char pos)
         (insertf "(interactive%s)"
                  (if (string-nil-or-empty-p arg-str)
                      ""
                    (format " \"%s\""
                            (string-join
                             (->> arg-str
                                  (string-split " ")
                                  (mapcar (| format "s%s: " %)))
                             "\\n"))))
         (indent-region (line-beginning-position) (line-end-position)))
       (error "unable to parse arglist"))))

(defun insert-interactive-completing-read ()
  "Generate an (interactive) declaration using `completing-read' for each field."
  (interactive)
  (let ((pos (point)))
    (re-search-backward "(defun +[^ ]+ +(\\(.*?\\))")
    (if-let (arg-str (match-string-no-properties 1))
       (progn
         (goto-char pos)
         (insertf "(interactive%s)"
                  (if (string-nil-or-empty-p arg-str)
                      ""
                    (format " (list %s)"
                            (string-join
                             (mapcar (| format "(completing-read \"%s: \" '() nil t)" %)
                                     (string-split " " arg-str))
                             "\n"))))
         (indent-region pos (point)))
       (error "unable to parse arglist"))))

(defun set-default-directory (dir)
  (interactive "fdir: ")
  (setq default-directory dir))

(defun jump-to-abbrev (handler abbrev table)
  (funcall handler (assoc1 abbrev table)))

;;
;; Work on this.
;;
;; (defun build-jump-fn (abbreviations table key jump-fn)
;;   (let ((abbrev (gensym)))
;;     `((progn )
;;       (defun jump-to-shell (,abbrev)
;;         (interactive (list
;;                       (completing-read "abbrev: " ',abbreviations)))
;;         (jump-to-abbrev ,jump-fn ,abbrev ',table))
;;        (global-set-key ,key #'jump-to-shell))))


;; I was going to make this a loop, but I've spent too much
;; time on it already.
;;
;; It seems like I should really be able to do this with a function,
;; but lexical stuff wasn't quite working.
;;
(defun mls--magit-branch-view (dir)
  (with-temp-buffer
    (set-default-directory dir)
    (magit-show-refs-head)))

(defmacro setup-jump-to-abbrev (abbrev-table)
  (let* ((table (eval abbrev-table))
         (abbreviations (mapcar #'car table))
         (abbrev (gensym)))
    `(progn
       (defun jump-to-shell (,abbrev)
         (interactive (list
                       (completing-read "abbrev: " ',abbreviations nil t)))
         (jump-to-abbrev #'shell-open-dir ,abbrev ',table))
       (global-set-key "\C-xg" #'jump-to-shell)

       (defun jump-to-dired (,abbrev)
         (interactive (list
                       (completing-read "abbrev: " ',abbreviations  nil t)))
         (jump-to-abbrev #'dired ,abbrev ',table))
       (global-set-key "\C-x\C-g" #'jump-to-dired)

       (defun jump-to-magit (,abbrev)
         (interactive (list
                       (completing-read "abbrev: " ',abbreviations nil t)))
         (jump-to-abbrev #'magit-status ,abbrev ',table))
       (global-set-key "\C-x\M-g" #'jump-to-magit)
       (defun jump-to-magit-branch (,abbrev)
         (interactive (list
                       (completing-read "abbrev: " ',abbreviations  nil t)))
         (jump-to-abbrev #'mls--magit-branch-view ,abbrev ',table))
       (global-set-key "\C-x\M-b" #'jump-to-magit-branch)
       (defun yank-abbrev-path (,abbrev)
         (interactive (list
                       (completing-read "abbrev: " ',abbreviations  nil t)))
         (jump-to-abbrev #'kill-new ,abbrev ',table))
       (defalias 'yap #'yank-abbrev-path)

       (defun insert-abbrev-path (,abbrev)
         (interactive (list
                       (completing-read "abbrev: " ',abbreviations  nil t)))
         (jump-to-abbrev #'insert ,abbrev ',table))
       (defalias 'iap #'insert-abbrev-path))))

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
                  (num (string-to-number num-str)))
          (setq total (+ total num))))
      total)))

(defun sum-col-region (begin end)
  "Add up numbers in a region of text."
  (interactive "r")
  (message "%d" (sum-col-region-fn begin end)))

(defvar m-bookmark-table (ht))

(defun open-url-tabs (urls)
  "Open a list of `urls' in one window with tabs."
  (destructuring-bind (first &rest rest) urls
    (browse-url first t)
    (sleep-for .5)
    (dolist (url rest)
      ;; You must pass nil, so it doesn't use the default
      ;; value for browse-url-new-window-flag
      (browse-url url nil)
      (sleep-for .2))))

(defun list-bookmark-data ()
  (ht-to-alist m-bookmark-table))

(defmacro make-bookmark (name url &rest urls)
  (message "Define bookmark %s" name)
  (assert (symbolp name))
  ;; Make sure this isn't going to smash anything.
  (assert (not (and (boundp name) (fboundp name))))

  (assert (or (symbolp url) (stringp url)))

  (dolist (u urls)
    (assert (or (symbolp u) (stringp u))))

  (ht-set! m-bookmark-table name url)
  (with-gensyms (u)
    `(defun ,name ()
       ,(format "The `%s' function opens up the %s url.%s" name url
                (if (> (length urls) 1)
                    (format "  And %d others." (length urls))
                  ""))
       (interactive)
       (open-url-tabs '(,url ,@urls)))))

(defun show-web-bookmark-data ()
  (interactive)
  (with-overwrite-buffer-pp "+bookmarked-urls+"
    (list-bookmark-data)))

(defmacro make-orgfile-bookmark (function-name file-path)
  "Define an interactive `FUNCTION-NAME' that opens `FILE-PATH'"
  (assert (not (and (boundp function-name) (fboundp function-name))))
  (assert (stringp file-path))

  `(defun ,function-name ()
     ,(format "Open the `%s' file in org-mode." function-name)
     (interactive)
     (find-file ,(expand-file-name file-path))
     (org-mode)))

(defun replace-string-in-region (begin end new-string)
  "Given a region defined with begin and end, replace
   That region with the new-string"
  (message (format "got new string: %s" new-string))
  (delete-region begin end)
  (goto-char begin)
  (insert new-string))

(defun add-password-prompt-check (check)
  (unless (cl-search check comint-password-prompt-regexp)
    (setf comint-password-prompt-regexp
          (concat comint-password-prompt-regexp check))))

(cl-defun read-user-password (prompt cache-key &optional (verify-password-fn #'identity))
  "Read and cache a password from the user."
  (if (password-in-cache-p cache-key)
      (password-read-from-cache cache-key)
    (progn
      (let ((password (cl-loop for password = (password-read prompt)
                               when (funcall verify-password-fn password)
                               return password)))
        (password-cache-add cache-key password)
        password))))

(defun title-caps-to-underbar (begin end)
  (interactive "r")
  (let ((case-fold-search nil))
    (replace-string-in-region begin end
      (replace-regexp-in-string
        "\\([A-Z]\\)"
        (| concat "_" (downcase %))
        (buffer-substring-no-properties begin end)))))

;; Emacs' calculator might already have this.
(defun linear-regression ()
  ;; http://en.wikipedia.org/wiki/Simple_linear_regression
  )

(defun exponential-moving-avg (smoothing-constant nums)
  (reverse
   (cl-reduce (fn (out-list new)
                (let* ((old (car out-list))
                       (diff (- new old))
                       (update (* (- 1 smoothing-constant) diff)))
                  (cons (+ old update) out-list)))
              (cdr nums)
              :initial-value (list (car nums)))))

(defun url-join (&rest args)
  (cl-flet ((clean-element (elm)
           (let ((trimmed (string-trim-chars elm "\\/" "\\/")))
             (if (string-ends-with "://" elm)
                 (concat trimmed "/")
               trimmed))))
    (string-join (mapcar #'clean-element args) "/")))

(defun url-basename (url)
  "Return the base component of the path of the given `url'."
  (basename (car (url-path-and-query (url-generic-parse-url url)))))

(defun url-join-list (elm)
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

(defun clear-buffer (buffer)
  "Clear the contents of the named buffer."
  (with-current-buffer buffer
    (delete-region (point-min) (point-max))))

(defmacro pushd (dir &rest body)
  "Run the body in this new default directory"
  (declare (indent defun))
  (with-gensyms (gold-dir gnew-dir)
    `(let ((,gnew-dir ,dir)
           (,gold-dir default-directory))
       (unwind-protect
           (progn
             (setf default-directory (normalize-dir-path
                                      (if (file-name-absolute-p ,gnew-dir)
                                          ,gnew-dir
                                        (path-join ,gold-dir ,gnew-dir))))
             ,@body)
         (setf default-directory ,gold-dir)))))

(cl-defmacro with-tempdir ((&key (root-dir "/tmp")
                                 leave-dir
                                 delay-cleanup-sec)
                        &rest body)
  "Run body inside a temporary directory.  It uses
   a uuid-4 for the directory name.

   It will be cleaned up unless `:leave-dir' is specified.
   `:root-dir' controls where the tempdir is created.
   "
  (declare (indent defun))

  (with-gensyms (root dir-name gdelay-cleanup-sec gleave-dir gresult)
      `(let* ((,root ,root-dir)
              (,root (or ,root default-directory))
              (,dir-name (path-join ,root (uuidgen-4)))
              (,gdelay-cleanup-sec ,delay-cleanup-sec)
              (,gleave-dir ,leave-dir))
         (unwind-protect
             (progn
               (make-directory ,dir-name t)
               (pushd ,dir-name
                 (let ((,gresult
                        (progn ,@body)))
                   (when ,gdelay-cleanup-sec
                     (sleep-for ,gdelay-cleanup-sec))
                   ,gresult)))
           (when (and (not ,gleave-dir)
                      (file-exists-p ,dir-name))
             (delete-directory ,dir-name t))))))

(cl-defun generate-temp-file-name (&key (root-dir "/tmp"))
  (path-join root-dir (uuidgen-4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Math Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun byte-conv-fn (in in-units out-units)
  (let* ((conv-table '((B . 1)
                       (KB . 2)
                       (MB . 3)
                       (GB . 4)
                       (TB . 5)
                       (PB . 6)
                       (EB . 7)))
         (unit-diff (- (assoc1 in-units conv-table)
                       (assoc1 out-units conv-table)))
         (conv-amount (expt 1024 (abs unit-diff))))

    (if (>= unit-diff 0)
        (* in conv-amount)
      (/ in conv-amount))))

(defun time-conv-fn (in in-units out-units)
  (let ((conv-table '((nano . (0.001 micro))
                      (micro . (0.001 mil))
                      (mil . 0.001)
                      (s . 1)
                      (m . (60 s))
                      (h . (60 m))
                      (d . (24 h))
                      (mon . (30 d))
                      (y . (365 d))
                      (decade . (10 y))
                      (century . (100 y)))))))

;; (byte-conv (5 MB) B)
(cl-defmacro bconv ((in units) out-units)
  `(byte-conv-fn ,in ',units ',out-units))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ini files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst ini-section-template
  "#
# %s
#
[%s]
%s")

(defun ini-string (ini-lisp)
  ;;
  ;; The lisp code should be a alist of sections:
  ;; as in:
  ;; (dosbox .
  ;;           (language . nil)
  ;;           (captures . something)
  (cl-loop for (section . values) in ini-lisp
           concat (format ini-section-template
                          (assoc-get 'comment values "Section")
                          section
                          (cl-loop for (key . value) in values
                                   concat (format "%s=%s\n" key (if value value ""))))))

(defun ini-write (ini-lisp path)
  (barf (ini-string ini-lisp) path))

(defun ini-parse (str)
  (cl-loop for (key . value) in (mapcar (fn (line)
                                            (acond
                                             ((string-find "\\[\\(.+?\\)\\]" line)
                                              (cons :section (cl-first it)))
                                             ((string-find "\\([^ =]+\\) *= *\\([^ =]+\\)" line)
                                              ;; line
                                              (destructuring-bind (key value) it
                                                (cons key value)))
                                             (t
                                              ;; skip
                                              )))
                                          (string->list str))
           with sections = '()
           do (progn
                (cond
                 ((eql :section key)
                  (pushcons value '() sections))
                 ((not sections)
                  (pushcons key value sections))
                 ((let ((section-list (cdr (cl-first sections))))
                    (or (not section-list)
                        (consp section-list)))
                  (pushcons key value (cdr (cl-first sections))))
                 (t
                  (pushcons key value sections))))
           finally (return sections)))


(defun ini-load-file (path)
  "Parse an .ini file and return its contents as an alist."
  (ini-parse (slurp path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OSX Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mount-dmg (path)
  (run "hdiutil" "attach" path))

(defun umount-dmg (path)
  (run "hdiutil" "detach" path))

(cl-defun make-smb-url (&key user host path)
  (assert host)

  (let ((url "smb://"))
    (when user
      (setf url (concat url user "@")))
    (when host
      (setf url (concat url host "/")))
    (when path
      (setf url (concat url path)))
    url))

(cl-defun mount-smbfs (host remote-path local-path
                            &key user password domain dir-mode file-mode options new-session)
  "Run the osx mount_smbfs command."
  (let ((args nil)
        (auth nil))
    (when user
      (setf auth user))
    (when password
      (assert user)
      (setf auth (concat auth (format ":%s" password))))

    (when options
      (append! args (list "-o" options)))

    (when dir-mode
      (append! args (list "-d" dir-mode)))

    (when file-mode
      (append! args (list "-f" file-mode)))

    (when new-session
      (append-atom! args "-s"))

    (append-atom! args (format "//%s%s%s%s"
                               (if domain (format "%s;" domain) "")
                               (if auth (format "%s@" auth) "")
                               host
                               (if remote-path (format "/%s" remote-path) "")))
    (append-atom! args local-path)

    (do-cmd "mount_smbfs" args)))

(defun list-mount-points ()
  "Return an alist of each mounted file/directory (see the return value of mount)."
  (cl-loop for line in (string->list (run-to-str "mount"))
           for split_line = (string-find "\\(.+?\\) on \\(.+?\\) (\\([^)]+\\))" line)
           do (assert (and split_line (= 3 (length split_line))))
           collect (map 'list #'cons '(:location :mount-point :flags) split_line)))

(defun umount-folder (folder)
    (interactive (list (completing-read "folder: " (mapcar (| assoc1 :mount-point %) (list-mount-points)))))
    (run "umount" folder))

(defun show-mount-points ()
  "Dump mount point information into a buffer."
  (interactive)
  (with-overwrite-buffer-pp "+mount-points+"
    (list-mount-points)))

(defun osx-screen-lock ()
    "Lock the screen immediately."
    (interactive)
    (let ((frameworks-screen-saver '("/System/Library/Frameworks/ScreenSaver.framework/Resources/ScreenSaverEngine.app/Contents/MacOS/ScreenSaverEngine"))
          (core-services-screen-saver '("/System/Library/CoreServices/ScreenSaverEngine.app/Contents/MacOS/ScreenSaverEngine"))
          (cgsession-saver '("/System/Library/CoreServices/Menu Extras/User.menu/Contents/Resources/CGSession" "-suspend")))

      (message "See if we need to start the screen saver.")
      (if (not (process-is-running-regex-p "ScreenSaverEngine$"))
          (cl-loop for cmd-list in (list frameworks-screen-saver core-services-screen-saver cgsession-saver)
                   when (file-exists-p (cl-first cmd-list)) do
                   (destructuring-bind (cmd &rest args) cmd-list
                     (message "Start screen saver: proc running: %s cmd: %s" (process-is-running-regex-p "ScreenSaverEngine$") cmd)
                     (apply #'run-bg cmd args)
                     (return)))
        (message "Don't start the screen saver, it's already running."))))

;; Add a thread here, so I can kill and restart the thread.
(let ((screen-lock-enabled t))
  (defun osx-screen-lock-regulated ()
    (message "try screen lock.  screen lock enabled: %s" screen-lock-enabled)
    (when screen-lock-enabled
      (osx-screen-lock)))

  (defun osx-screen-lock-enabled ()
    (interactive)
    (message "Screen lock currently enabled")
    (setf screen-lock-enabled t))

  (defun osx-screen-lock-disabled ()
    (interactive)
    (message "Screen lock currently disabled")
    (setf screen-lock-enabled nil))

  (defun osx-screen-lock-status ()
    screen-lock-enabled)

  (defun osx-screen-lock-renable-later (min)
    (osx-screen-lock-disabled)

    (assert (and (integerp min)) (> min 0))

    (run-at-time (format "%smin" min) nil #'osx-screen-lock-enabled)
    (message "Enable later status: %s" (osx-screen-lock-status))))

(defun disable-lockout-temporarily (min)
  (interactive "smin: ")
  (osx-screen-lock-renable-later (string-to-number min)))

(defun osx-sleep-now ()
  "Put the system to sleep immediately."
  (run "pmset" "sleepnow"))

(defun osx-sleep-soon (mins)
  (interactive "smins: ")
  (run-at-time (format "%s min" mins) nil #'osx-sleep-now))

(defun osx-num-cores ()
  "Get the number of logical cores on an osx system."
  (string-to-number (string-trim (run-to-str "sysctl" "-n" "hw.ncpu"))))

(defun osx-num-cores-physical ()
  "Get the number of logical cores on an osx system."
  (string-to-number (string-trim (run-to-str "sysctl" "-n" "hw.physicalcpu"))))

(defun kill-dock-osx ()
  (interactive)
  (run "killall" "Dock"))

(defun kill-finder ()
  (interactive)
  (run "killall" "Finder"))

(defun disable-screen-lockout-temporarily (min)
  (interactive "smin: ")
  (osx-screen-lock-renable-later (string-to-number min)))

;; To edit apple script, one option is to run: ScriptEditor.app
(defun run-osascript (script &rest args)
  (with-tempdir (:root-dir "/tmp")
    (let ((script-path "apple-script.txt"))
      (barf script script-path)
      (do-cmd (append (list "osascript" script-path) args) :throw t :stdout 'string))))

(defun osascript-quote-str (str)
  (concat "\"" (escape-string str "\"\\") "\""))

(defconst +osx-path-to-firefox+ "/Applications/Firefox.app/Contents/MacOS/firefox")

(defun macos-get-current-screen-resolution ()
  (string-trim (assoc1 :stdout (run-osascript "tell application \"Finder\" to get bounds of window of desktop"))))

(defun init-quicktime-movie (path)
  (do-cmd (list "killall" "QuickTime Player"))
  (sleep-for 5)

  (run-osascript (format "tell application \"QuickTime Player\"
    open %s
    set myMovie to document 1
    tell myMovie
        %s
        play
        activate
    end tell
end tell" (quote-str path) ""))
  (sleep-for 15)
)

(defun open-quicktime-movie (path &optional start-time-sec)
  (run-osascript (format "tell application \"QuickTime Player\"
    open %s
    set myMovie to document 1
    tell myMovie
        %s
        play
        activate
    end tell
end tell
" (quote-str path) (if start-time-sec
                                 (format "set current time to %d" start-time-sec)
                               ""))))

(defun osx-move-to-trash (files)
  (do-cmd-succeeded-p (run-osascript (format "tell app \"Finder\" to move {%s} to trash"
                         (string-join
                          (mapcar (fn (path)
                                    (format "the POSIX file %s"
                                            (osascript-quote-str
                                             (if (file-name-absolute-p path)
                                                 path
                                               (path-join default-directory path)))))
                                  (to-list files))
                          ", ")))))

(defun get-chrome-path ()
  (cl-ecase system-type
    (darwin "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
    (linux "google-chrome")))

(defun browse-url-chrome (url &optional new-window)
  (interactive "surl: ")
  (let ((browse-url-generic-program "/opt/google/chrome/google-chrome"))
    (browse-url-generic url new-window)))

(defun browse-url-chrome-osx (url &optional new-window)
  "A `browse-url' function for Chrome on OSX.  Normally we end up with
   whatever the 'open' command does, but for me I like when it opens a new
   window.  This function provides that feature.

   To enable it as the default browser do:
   (setq browse-url-browser-function 'browse-url-chrome-osx)
   (setq browse-url-new-window-flag t)"

  (interactive (browse-url-interactive-arg "URL: "))
  (do-cmd `("/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
            ,@(if new-window
                  (list "--new-window" url)
                (list url))))
  (run "open" "-a" "Google Chrome"))

(defun browse-url-firefox-osx (url &optional new-window)
  "A `browse-url' function for firefox on OSX.  Normally we end up with
   whatever the 'open' command does, but for me I like when it opens a new
   window.  This function provides that feature.

   To enable it as the default browser do:
   (setq browse-url-browser-function 'browse-url-firefox-osx)
   (setq browse-url-new-window-flag t)"
  (interactive (browse-url-interactive-arg "URL: "))
  (do-cmd `(,+osx-path-to-firefox+ ,@(when new-window
                                       '("--new-window"))
                                   ,url)))

(defun osx-open (&rest cmd)
  "OSX's open command can be used for many things, including
   connecting to servers and vnc, etc."

  (apply #'run "open" cmd))

(defun osx-open-vnc (ip)
  "Open a VNC using OSX's internal VNC.  `ip' can be a hostname
   or an ip address.

   Note, this may require the vnc app to already be open?
   I'm not completely clear on that.
   "
  (osx-open (format "vnc://%s" ip)))

(defun du (dir)
  (string-to-int
   (assoc1 'size (last-car
                  (csv-split-text
                   (run-to-str "du" "-k" "-d" "0" "-c" dir)
                   :split-regex "[ \t]" :has-header-line nil :field-names '(size name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dumpenv ()
  "Dump the current environment into an alist."
  (mapcar (lambda (var)
            (if-let (key-value (parse-env-var var))
                (apply #'cons key-value)
              (cons var nil)))
          process-environment))

(defun dumpenv-debug ()
  (interactive)
  (cl-loop for (k . v) in (dumpenv) do (message "env: %s %s" k v)))

;; I haven't ever used this, do I need it?
;; would something like it make the "with-env" or
;; "with-venv" functions easier to write?
(defun in-new-env (env callback-fn)
  (let ((old-env-values '()))

    ;; Push the new environment values.
    (mapc (fn ((name . new-value))
              (push (cons name (getenv name)) old-env-values)
              (setenv name new-value))
          env)

    (ignore-errors
      (funcall callback-fn))

    (mapc (fn ((name . old-value))
              (setenv name old-value))
          old-env-values)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Async Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defun do-list-async (list &key fn (cb (lambda ())))
  "A helper/wrapper for when you want to do work on a list with a function that takes a callback.


`list': a list of things that will get passed one at a time to the callback.

"
  (if list
    (cl-labels ((handler-fn (&rest args)
                  (if list
                    (funcall fn (pop list) #'handler-fn)
                    (funcall cb))))
      (funcall fn (pop list) #'handler-fn))
    (funcall cb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun git-remote-origin-url ()
  (interactive)
  (if-let (url (string-trim (run-to-str "git" "config" "--get" "remote.origin.url")))
      url
    (error "Unable to find git remote.origin.url.  Is this a git repo?")))

(defun git-remote-add-origin (origin)
  (run "git" "remote" "add" "origin" origin))

(defun git-remote-list ()
  (string->list (run-to-str "git" "remote")))

(defun git-remote-remove (name)
  (run "git" "remote" "remove" name))

(defun git-remote-remove-all ()
  (dolist (remote-name (git-remote-list))
    (message "Removing remote %s" remote-name)
    (git-remote-remove remote-name)))

(defun git-first-commit ()
  (string-trim (run-to-str "git" "rev-list" "--max-parents=0" "HEAD")))

;; Make this less dumbx
;; (defun git-log ()
;;   (run-to-str "git" "log"))

(defun git-push-origin-master ()
  (run "git" "push" "-u" "origin" "master"))

(defun git-push-sha (sha &optional branch)
  (let ((branch (or branch "master")))
    (run "git" "push" "-u" "origin" (format "%s:refs/heads/%s" sha branch))))

(defun git-repo-exists-p (path)
  (when (and (file-exists-p path)
             (file-exists-p (path-join path ".git")))
    (pushd path
      ;; Will this check be to slow on large repos?
      (run-is-true "git" "status" "."))))

(defun git-push-current-branch (&key set-upstream)
  ""
  ;;
  ;; git push -v origin test:refs/heads/test
  ;;

  (let ((target "origin"))
    (apply #'run `("git" "push"
                   ,@(when set-upstream
                       (list "--set-upstream"))
                   ,target
                   ,(format "%s:%s" (git-current-branch) (git-current-branch-ref))))))

(defun git-get-upstream-branch ()
  (string-trim (run-to-str "git" "rev-parse" "--abbrev-ref" "--symbolic-full-name" "@{u}")))

(defun git-get-all-branch-info ()
  (let ((field-names '("refname" "upstream")))
    (csv-split-text (run-to-str "git" "branch" (concat "--format=" (string-join
                                                                    (mapcar (fn (field)
                                                                             (format "%%(%s)" field))
                                                                            field-names)
                                                                    ",")))
                    :field-names field-names
                    :has-header-line nil)))

(defun git-get-branch-info (branch-name)
  (first
   (filter (fn (info)
             (string-ends-with branch-name
                               (assoc1 "refname" info)))
           (git-get-all-branch-info))))

(defun git-branch-tracks-remote-p ()
  (let* ((current-branch-ref (git-current-branch-ref))
         (branch-info (git-get-branch-info current-branch-ref))
         (upstream (assoc1 "upstream" branch-info)))
    (let ((potential-remote-branch-name (if (string-ends-with "heads/master" current-branch-ref)
                                            "remotes/origin/master"
                                          (basename current-branch-ref))))
      (not (not (string-ends-with potential-remote-branch-name upstream))))))

(cl-defun git-sync-branch (&optional (remote-branch "origin/master"))
  "Attempt to bring the current git branch up to date.

If you have a remote branch, you need to merge, if you don't have remote changes, you can
rebase."
  (interactive)
  (let ((local-branch (git-current-branch)))
    (cl-assert (not (string-ends-with "master" local-branch)) nil "Don't run this command on master.")
    (run "git" "checkout" remote-branch)
    (run "git" "fetch" "-p" "origin")
    (run "git" "merge" remote-branch)
    (run "git" "checkout" local-branch)
    (if (git-branch-tracks-remote-p)
        (run "git" "merge" remote-branch)
      (cl-destructuring-bind (origin branch)
          (string-find "^\\([^/]+\\)/\\(.+\\)$" remote-branch)))
    (magit-status)))

(cl-defun git-sync-branch-from-master ()
  "Attempt to bring the current git branch in sync with origin/master.

If you have a remote branch, you need to merge.  If you don't have remote changes
you can rebase."
  (interactive)

  (let ((local-branch (git-current-branch)))
    (cl-assert (not (string-ends-with "master" local-branch)) nil "Don't run this command on master.")
    (run "git" "fetch")
    (if (git-branch-tracks-remote-p)
        (run "git" "merge" "origin/master")
      (run "git" "pull" "--rebase" "origin/master" local-branch))
    (magit-status)))

(defun git-get-origin-info ()
  (let* ((urlobj (url-generic-parse-url (git-remote-origin-url)))
         (pandq (cl-first (url-path-and-query urlobj)))
         (repo (file-name-sans-extension (basename pandq)))
         (path (file-name-directory pandq)))

    ;; It might be nicer to return an alist.
    `((host . ,(url-host urlobj))
      (path . ,path)
      (repo . ,repo))))

(defun git-symbolic-ref (&rest args)
  "Run the git symbolic-ref command, see the man page for details."
  (string-trim (apply #'run-to-str "git" "symbolic-ref" args)))

(defun git-current-branch-ref ()
  "Get the full ref name of the current git branch."
  (git-symbolic-ref "HEAD"))

(defun git-current-branch ()
  "Get the short name of the git branch in the current repo."
  (interactive)
  (git-symbolic-ref "--short" "HEAD"))

(defun git-project-root ()
  "Return the root of a git project."
  (string-trim (run-to-str "git" "rev-parse" "--show-toplevel")))

(defun git-init-repo (path &optional bare)
  (let ((args (list path)))
    (when bare
      (append-atom! args "--bare"))
    (apply #'run "git" "init" args)))

(cl-defun git-branch-create (branch-name &key start-point track force)
  "Create a branch in the current git repo."
  (assert (git-within-git-repo-p) nil "This command must be run inside a git repo.")

  (let ((options `(,@(when track
                       (list "--track"))
                   ,@(when force
                       (list "-f")))))
    (apply #'run `("git" "branch" ,@options ,branch-name ,@start-point))))

(defun git-branch-list ()
  "Return a list of git branches."
  (assert (git-within-git-repo-p) nil "This command must be run inside a git repo."))

(cl-defun git-checkout (branch &key create-branch)
  "Switch to a new branch"
  (assert (git-within-git-repo-p) nil "This command must be run inside a git repo.")
  (apply #'run `("git" "checkout" ,@(when create-branch
                                      (list "-b"))
                 ,branch)))

(defun git-commit (message)
  (run "git" "commit" "-m" message))

(cl-defun git-commit-buffer (&key message)
  (let ((message (or message
                     (format "Committing changes to %s" (buffer-name)))))
    (run "git" "add" (buffer-file-name))
    (git-commit message)))

(cl-defun git-commit-changes (path &key (message "Save current files."))
  (pushd path
    (run "git" "add" ".")
    (git-commit message)))

(defun git-rev-parse-is-inside-working-tree ()
  (do-cmd-succeeded-p (do-cmd (list "git" "rev-parse" "--is-inside-work-tree"))))

;; TODO: rename this to indicate it's a test.  It's hard to find.
(defun git-within-git-repo-p (&optional dir)
  "Is the current or provided directory inside a valid git repo?"
  (pushd (or dir default-directory)
    (git-rev-parse-is-inside-working-tree)))

(defun git-clone (clone-url)
  "Clone a git repo into the current directory."
  (run "git" "clone" clone-url))

(defcustom git-repo-remote-dir nil
  "A variable pointing to a 'local' directory for storing git repos.")

(defcustom git-repo-local-root-dir nil
  "A variable that specifies the root of local git repos.  The idea
is that the remote-setup dir will use this to create folders in
the remote git directory.   That way you can have dir/project not
conflict with dir/subdir/project.")

(defun git-init-setup-remote-repo-dir (dir)
  ;; I could try prompting for the password and automounting
  (assert (file-exists-p git-repo-remote-dir) t
          (format "Don't forget to mount %s" git-repo-remote-dir))

  (let* ((remote-path-prefix (cl-set-difference (f-split (expand-file-name dir))
                                                (f-split git-repo-local-root-dir) :test #'equal))
         (repo-name (last-car remote-path-prefix)))
    (assert (not (string-nil-or-empty-p repo-name)))

    (message "Add repo %s to our remote repository." repo-name)
    (let ((remote-repo-dir (path-join git-repo-remote-dir
                                      (concat (apply #'f-join remote-path-prefix) ".git"))))
      (message "Init remote repo: %s" remote-repo-dir)
      (git-init-repo remote-repo-dir t)

      (pushd dir
        (if (git-within-git-repo-p)
            (progn (message "Set origin of existing repo.")
                   (git-remote-remove-all)
                   (git-remote-add-origin remote-repo-dir)
                   (git-push-origin-master))
          (progn
            (message "Initialize non-git repo dir: %s" dir)
            (git-init-repo ".")
            (run "git" "add" ".")
            (git-commit-changes "." :message "Initial repo commit.")
            (git-remote-remove-all)
            (git-remote-add-origin remote-repo-dir)
            (git-push-origin-master)))))))

(defun git-init-remote-repo ()
  "Create a master git repo in the `git-repo-remote-dir' directory."
  (interactive)
  (git-init-setup-remote-repo-dir
   (if (git-within-git-repo-p)
       (git-project-root)
     (if (y-or-n-p (format "Is %s the base of the repo?" default-directory))
         default-directory
       (error "Please run this command from the root of the repo!")))))

(defun git-repo-remote-dir-list-repos--core ()
  (mapcar (fn (path)
            (cons (file-name-base path) path))
          (list-directory-entries git-repo-remote-dir :full t :dirs-only t :match "\\.git$")))

(defun git-repo-remote-dir-list-repos ()
  (interactive)

  (with-overwrite-buffer-pp (format "+remote-repo-dir-%s-repos+" git-repo-remote-dir)
    (git-repo-remote-dir-list-repos--core)))

(defun git-repo-remote-dir-clone (target-dir repo)
  (interactive (list (read-string "target-dir: ")
                     (completing-read "repo: " (assoc-keys (git-repo-remote-dir-list-repos--core))
                                      nil t)))

  (let ((repo-table (git-repo-remote-dir-list-repos--core)))
    (pushd target-dir
      (git-clone (assoc1 repo repo-table)))))

(defun git-status ()
  (cl-loop for line in (string->list (run-to-str "git" "status" "--short"))
           collect (destructuring-bind (status path)
                       (string-find "\\([^\s]+\\) \\(.*\\)" line)
                     (cons status path))))

(defun git-modified-files ()
  (mapcar #'string-trim (string->list (run-to-str "git" "ls-files" "-m"))))

(defun git-modified-file (path)
  (not (string-nil-or-empty-p (run-to-str "git" "ls-files" "-m" path))))

(defun git-remove-sensitive-file ()
  (interactive)
  (let ((file-path (buffer-file-name)))
    (when (y-or-n-p (format "Are you sure you want to purge %s?: " file-path))
      (do-cmd-async
       (list "git" "filter-branch" "--force" "--index-filter"
             (format "git rm --cached --ignore-unmatch %s" file-path)
             "--prune-empty" "--tag-name-filter" "cat" "--" "--all")
       :callback-fn (fn (res)
                      (when (y-or-n-p "Post removal to remote?: ")
                        ;; To do tags, do:
                        ;; git push origin --force --tags
                        (do-cmd "git" "push" "origin" "--force" "--all")))
       :throw t))))

(defun git-file-path-from-repo-root (path)
  "Get the path of a file relative tot he root of the current git repo."
  (cl-assert (file-name-absolute-p path))

  (let ((repo-root (git-project-root)))
    (cl-assert (directory-file-under-dir repo-root path))
    (string-remove-prefix repo-root path)))

(defun git-buffer-path-from-repo-root ()
  "Return the path from the root of the git repo to the file held in the current buffer."
  (git-file-path-from-repo-root (buffer-file-name)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Groovy Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun open-groovy-shell (&optional dir)
  (interactive)

  (assert (which "groovysh"))

  (let ((target-dir (or dir default-directory)))
    (shell-open-run-command target-dir (list "groovysh"))

    (rename-buffer (generate-new-buffer-name (format "*groovy-shell-%s-groovy-shell*"
                                                     (file-relative-name target-dir (expand-file-name "~")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun yank-to-file-location-python ()
  "Take the path and line number of the current cursor position
and put it into the kill ring.

The idea is that this is an easy way to set a break point in a
python debugging session."
  (interactive)
  (kill-new (format "b %s:%d" (buffer-file-name) (line-number-at-pos))))

(defconst python-import-regex
  "^\s*\\(import\s+[_a-zA-Z0-9]+\\|from\s+[_a-zA-Z0-9]+\s+import\s+[_a-zA-Z0-9]+\\)"
  "Searches for 'from <identifier> import <identifier>' or 'import <identifier>' forms")

(defun jump-to-imports-python ()
  "Jump up to the lowest import statement for python.

   Also record our current location so we can jump back.
   it's saved in register ?p, so you can type
   C-x r j p to get back.
   "
  (interactive)

  (message "%s" (point-marker))
  (point-to-register ?p)
  (re-search-backward python-import-regex nil t)
  (goto-char (if-let (pos (match-beginning 0))
                 (goto-char pos)
               (point-min))))

(defun guess-python-version-3 ()
  "Attempt to guess if a buffer contains python2 or python3 code"
  (interactive)

  (message "Guess python version.")

  (condition-case nil
      (progn
        (message "Attempt to find the virtualenv's python in default dir %s" default-directory)
        (let ((python-version-string (find-venv-python-version (git-project-root))))
          (message "Found python and its version is: %s" python-version-string)
          (python-version-is-python-3 python-version-string)))
    (error
     ;; For the error case, try to guess.
     (save-excursion
       ;; This is hacky, but it should work.
       ;; scan for regexes and score python3 vs 2.
       ;;
       ;; python3 indicators
       ;; Search for python3
       ;; Search for print()
       ;; class Foo:
       ;; nonlocal
       ;; {k: v for k, v in stuff}
       ;;
       ;; python2 indicators
       ;; xrange
       ;; class Foo(object):
       (message "Fallback to guessing")

       (let* ((python-NAME-regex "[A-Za-z_][A-Za-z0-9_]+")
              (python3-regex (list "#.*python3"
                                   "\s+print("
                                   "nonlocal"
                                   (format "{%s:\s*%s\s+for\s+"
                                           python-NAME-regex python-NAME-regex)))
              (python2-regex (list "#.*python[^3]"
                                   "xrange("
                                   "print\s+[^(]"
                                   (format "class\s+%s\s*(object)\s*:" python-NAME-regex)
                                   "\.iteritems("))
              (py3-score 0))
         (cl-flet ((run-searches (regexes score-fn)
                                 (dolist (regex regexes)
                                   (goto-char 0)
                                   (when (re-search-forward regex nil t)
                                     (message "match regex %s" regex)
                                     (funcall score-fn)))))
           (run-searches python3-regex (| cl-incf py3-score))
           (run-searches python2-regex (| decf py3-score)))

         ;; If the score is positive or zero, assume python3.
         (message "final score: %d" py3-score)
         (>= py3-score 0))))))

;; TODO(mscheinh): make this work with emacs 26
(defun update-flymake-mask (file-pattern func)
  "Update the flyname mask for `file-pattern'.  This removes any previous matches"
  (setq flymake-allowed-file-name-masks
        (cons (list file-pattern func)
              (remove-if (| equal file-pattern (cl-first %))
                         flymake-allowed-file-name-masks))))

(defun is-py-activation-p (path)
  (not (not (string-match "deactivate.+().+{" (slurp path)))))

(cl-defun find-py-activate-file (root-dir)
  "Search for an `activate' file in the current directory or above."

  (do ((check-dir root-dir (expand-file-name (path-join check-dir "..")))
       (activate-file))
      ((equal "/" check-dir))
    (setf activate-file (path-join check-dir "activate"))
    (when (and (file-exists-p activate-file)
               (is-py-activation-p activate-file))
      (cl-return-from find-py-activate-file activate-file))))

;; TODO: I should rename this
(cl-defun find-virtualenv-file (root-dir)
  "Search a Python repo for the most like venv \'activate\' file.

  `root-dir' is where to start the search."

  ;;
  ;; Only search this way if we know this is a git repo...
  ;; otherwise it might match stuff it shouldn't
  ;;
  (car (cl-first
        (sort (mapcar (fn (path)
                        (cons path (cond
                                    ((equal (normalize-dir-path (file-name-directory path))
                                            (normalize-dir-path root-dir)) 10)
                                    (t 0))))
                      (directory-files-recursively root-dir "^activate$"))
              (fn (a b) (> (cdr a) (cdr b)))))))

(defun parse-env-var (line)
  (string-find "^\\([^=]+\\)=\\(.+\\|\\)$" line))

(defun parse-env-lines (lines)
  (mapcar (fn (line)
            (when-let (key-value (parse-env-var line))
                (apply #'cons key-value)))
          (string->list lines)))

(defconst venv-required-vars '("VIRTUAL_ENV" "PATH"))

(defun find-venv-variables (root-dir)
  ;; TODO: share this code better.
  (if-let (activate-file (or (find-py-activate-file root-dir)
                              (find-virtualenv-file root-dir)))
      (with-tempdir (:root-dir "/tmp")
        (let ((input-path "find-python-script"))
          (barf (format "source %s; env" (cmd-to-shell-string (list activate-file)))
                input-path)
          (filter (| member (car %) venv-required-vars)
                  (parse-env-lines (run-to-str "bash" input-path)))))
    (debug "Unable to find virtualenv activation file in %s" root-dir)))

(defun find-venv-root (root-dir)
  (assoc1 "VIRTUAL_ENV" (find-venv-variables root-dir)))

(defun find-venv-binary (root-dir binary)
  (let ((bin-path (path-join (find-venv-root root-dir) binary)))
    (assert (file-exists-p bin-path))
    bin-path))

(defun find-venv-python-binary (root-dir)
  (find-venv-binary root-dir "bin/python"))

(defun find-venv-pip-binary (root-dir)
  (find-venv-binary root-dir "bin/pip"))

(defun in-venv-p ()
  "Are we currently inside a python virtualenv?"
  (assoc "VIRTUAL_ENV" (dumpenv)))

(defun python-return-version-string (python-bin)
  "Amusingly, python switched between stdout and stderr between versions 2
   and 3.  So account for that."

  (let ((res (do-cmd (list python-bin "--version") :stdout 'string :stderr 'string)))
    (string-trim
     (cond
      ((string-has-value-p (assoc1 :stderr res))
       (assoc1 :stderr res))
      ((string-has-value-p (assoc1 :stdout res))
       (assoc1 :stdout res))))))

(defun find-venv-python-version (root-dir)
  (mapcar #'string-to-number
          (string-find "[pP]ython \\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)"
                       (python-return-version-string
                        (find-venv-python-binary root-dir)))))

(defun python-version-is-python-3 (version)
  (= 3 (cl-first version)))

(defun find-venv-is-python-3 (root-dir)
  (python-version-is-python-3 (find-venv-python-version root-dir)))

(defun python-project-has-venv (root-dir)
  (condition-case nil
      (find-venv-variables root-dir)
    (error nil)))

(defun python-generate-pytest-call ()
  (save-excursion
    (re-search-backward "def \\([a-zA-Z-_][a-zA-Z0-9_-]*\\)(")
    (if-let (fn-name (match-string-no-properties 1))
        (format "pytest --disable-warnings --show-capture=all -svv %s::%s"
                (buffer-file-name) fn-name)
      (error "Unable to find python function name"))))

(defun python-setup-venv ()
  (interactive)
  (run "python3" "-m" "venv" ".venv/")
  (run "ln" "-s" ".venv/bin/activate" "activate"))

(defun python-reset-venv ()
  (interactive)
  (when (y-or-n-p (format "Reset virtual env in %s" default-directory))
    (message "Resetting virtual env in %s" default-directory)
    (let* ((venv-dir ".venv/")
           (activate-file "activate"))
      (run "rm" "-rf" venv-dir)
      (run "rm" "-f" activate-file)
      (run "rm" "-f" ".python-lsp-installed")
      (run "python3" "-m" "venv" venv-dir)
      (make-symbolic-link (path-join venv-dir "bin/activate") activate-file))))

(defun yank-to-pytest-fn ()
  "Generate a pytest string for a particular python test fuction, and store it
in the keyring."
  (interactive)
  (kill-new (python-generate-pytest-call)))

(defun open-pytest-fn-shv ()
  ""
  (interactive)
  (let* ((pytest-str (python-generate-pytest-call))
         (project-root (git-project-root))
         (project-name (basename project-root)))
    (open-shell-dir-venv project-root)
    (rename-buffer (generate-new-buffer-name (format "*venv-unit-test-%s-venv*" project-name)))
    (insert pytest-str)))

(defun open-shell-dir-venv (&optional dir)
  "Start a virtual env in the current repo."
  (interactive)

  (pushd (or dir default-directory)
    (switch-to-buffer (shell-open-dir default-directory))

    (let ((activate-link (or (find-py-activate-file default-directory)
                             ;; Why am I using (basename) here?
                             (find-virtualenv-file (basename (git-project-root))))))
      (unless activate-link
        (error "Unable to locate an activate link from directory %s" default-directory))

      (rename-buffer (generate-new-buffer-name (format "*venv-%s-%s-venv*" (basename default-directory) (buffer-name))))

      ;; Issue a command to setup the virualenv
      (goto-char (point-max))
      (insertf "source %s" activate-link)
      (comint-send-input nil t)
      (comint-clear-buffer)

      ;; We want to be sure to return the buffer we created.
      (current-buffer))))

(defun yank-venv-link (&optional dir)
  (interactive)

  ;; NOTE! This doesn't find the root of a repo.
  (let* ((root-dir (or dir default-directory))
         (activate-link (find-virtualenv-file root-dir)))
    (message "Found %s in root dir %s" activate-link root-dir)
    (kill-new activate-link)))

(defun python-get-project-root ()
  "Find the `root' of a python project."
  (if (git-within-git-repo-p)
      (git-project-root)
    (let ((python-root-files '("setup.py" "requirements.txt" "activate")))
      ;;
      ;; See if we can find setup.py or requirements.txt
      ;;
      (if-let (dir (cl-loop for parent in (file-list-parents default-directory)
                            when (cl-loop for file in python-root-files
                                          when (file-exists-p (path-join parent file))
                                            return t)
                            return parent))
          dir
        default-directory))))

(defvar virtualenv-saved-vars (let ((env (dumpenv)))
                                (mapcar
                                 (| cons % (cdr (assoc % env)))
                                      venv-required-vars)))

(defun set-exec-path-to-bash-path ()
  "Copy the current PATH into the emacs `exec-path'."
  (setf exec-path (string-split ":" (getenv "PATH") )))

(defmacro with-exec-path (new-parts &rest body)
  (with-gensyms (old-exec-path new-exec-path)
    `(let ((,old-exec-path (copy-list exec-path))
           (,new-exec-path (concatenate 'list (to-list ,new-parts) exec-path)))
       (unwind-protect
           (progn
             (setf exec-path ,new-exec-path)
             ,@body)
         (setf exec-path ,old-exec-path)))))

(defun activate-virtualenv-emacs (location)
  (let ((vars (find-venv-variables location)))
    (when (not (assoc-get "VIRTUAL_ENV" vars))
      (error "Virtualenv not found in %s" location))
    (dolist (var venv-required-vars)
      (setenv var (assoc1 var vars)))

    ;; Set the exec path, so call-process and what not match the shell.
    (set-exec-path-to-bash-path)
    (message "In virtualenv %s" (getenv "VIRTUAL_ENV"))))

(defun run-python-in-venv-dir (dir)
  "Run the special emacs python interpreter inside the venv in this dir."
  (interactive "Ddir: ")

  (pushd dir
    (let ((python-interpreter (path-join
                               (assoc1 "VIRTUAL_ENV"
                                       (find-venv-variables (git-project-root))) "bin/python")))
      (message "Run python interpreter: %s" python-interpreter)
      (run-python python-interpreter t))))

(defun run-python-in-venv ()
  "Run the special emacs python interpreter inside a venv associated with the buffer."
  (interactive)

  ;;
  ;; First, get to the root of the project, because that will make the imports nicer.
  ;;
  (run-python-in-venv-dir (python-get-project-root)))


(defun clear-virtualenv-emacs ()
  (mapc (fn ((name . value))
          (setenv name value)) virtualenv-saved-vars)

  ;; Makre sure exec path matches the regular PATH
  (set-exec-path-to-bash-path))

(defun restore-env-from-alist (alist)
  ;; clear the env first.
  (setf process-environment '())
  (loop for (k . v) in alist do (setenv k v)))

(defun setenv-from-alist (alist)
  (cl-loop for (k . v) in alist do (setenv k v)))

(defmacro with-venv (project-root &rest body)
  "Run some code within the venv for a given project root directory."
  (declare (indent defun))
  (with-gensyms (root-dir old-env)
    `(let* ((,root-dir ,project-root)
            (,old-env (copy-list process-environment)))
       (unwind-protect
           (progn
             (activate-virtualenv-emacs ,root-dir)
             ,@body)
         (progn
           ;; Should I have a cleaner way to do this?
           (setf process-environment ,old-env)
           (set-exec-path-to-bash-path))))))

(defmacro with-env-vars (env-vars-alist &rest body)
  "Run some code with a alist of environment variables set temporarily."
  (declare (indent defun))
  (with-gensyms (old-env alist)
    `(let ((,alist ,env-vars-alist)
           (,old-env (copy-list process-environment)))
       (unwind-protect
           (progn
             (setenv-from-alist ,alist)
             ,@body)
         (setf process-environment ,old-env)))))

(defun run-eval-python (python-code &rest do-cmd-args)
  (apply #'do-cmd (list "python3" "-c" python-code) do-cmd-args))

(defun run-python-pipe (input python-code)
  (assoc1 :stdout (run-eval-python python-code :input input :stdout 'string :throw t)))

(defun parse-yaml-file (path)
  (json-read-from-string (run-python-pipe path "import sys, json, yaml; print(json.dumps(yaml.load(sys.stdin, Loader=yaml.FullLoader)))")))

(defun json-pretty-print-file (path)
  (with-tempdir (:root-dir "/tmp")
    (let ((temp-file "pretty-file.js"))
      (do-cmd (list "python3" "-m" "json.tool")
              :throw t
              :input path
              :stdout (list :file temp-file))
      (rename-file temp-file path t))))

(defun pip-install (&rest library-options)
  (apply #'run "pip" "install" library-options))

(defun python-lsp-clear-cache ()
  ;; find . -name .python-lsp-installed -print0 | xargs -0 rm -f
  )

;; To use, install: sudo -H pip install python-language-server
;; and sudo -H pip3 install python-language-server
;;
;; check out: https://pypi.org/project/python-language-server/
;;
(defun python-lsp-setup-project (project-root)
  ;; TODO: should this auto-create a venv?

  (message "+++ Check lsp project in %s" project-root)
  (with-venv project-root
    (message "This is a git project, so activate the virtualenv if possible")
    (message "Install any needed dependencies in the virtualenv %s" (getenv "VIRTUAL_ENV"))

    ;; This is safe to do here, since we're in the venv.
    (let ((installation-finished-file (path-join project-root ".python-lsp-installed")))
      (message "Check to see if the install has already been run: %s %s"
               (file-exists-p installation-finished-file)
               installation-finished-file)
      (unless (and (file-exists-p installation-finished-file)
                   (which "pyls"))
        ;; Note: Don't install the project "pyls", that is something else.
        (run "pip" "install" "python-language-server[all]==0.36.2")
        (touch installation-finished-file)))))

;;
;; TODO: should I just have this return the directory?
;; the executable name is already known.
;;
(defun python-find-executable (exec-name &optional required-version)
  "Find the named executable preferably in the venv, otherwise
in the global python env if needed.

Algorithm:

    is this a single script or a full python project?
    full project: is_git or has its own venv
      point to the custom installed one
    else
      use the defaults."
  (cl-flet ((find-venv-exec-path ()
              (let* ((project-root (python-get-project-root))
                     (has-venv (find-virtualenv-file project-root))
                     (output (list exec-name "/usr/")))
                (when has-venv
                  (with-venv project-root
                    (which exec-name))))))
    (if-let (venv-path (find-venv-exec-path))
        venv-path
      exec-name)))

(defun python-lsp-get-config ()
  "Return the path to pyls and the virtualenv directory."

  ;; make sure everything is installed
  ;;
  ;; This is a little gross, as it is also required to make
  ;; yapf work.
  ;;
  (when (git-within-git-repo-p)
    (when-let (project-root (python-get-project-root))
      (python-lsp-setup-project project-root)))

  (let ((pyls-path (python-find-executable "pyls")))
    (list pyls-path (file-name-directory pyls-path))))

(defun current-virtualenv ()
  (getenv "VIRTUAL_ENV"))

(defun python-insert-class ())
(defun python-insert-import-region (begin end)
  (interactive "r")
  (save-excursion
    (beginning-of-buffer)
    (let ((import-name (buffer-substring-no-properties begin end))
          (import-start nil)
          (import-end nil))

      ;; find the first import block
      (re-search-forward "^import +[a-zA-Z0-9]+")
      (beginning-of-line)
      (setf import-start (point))

      (message "Adding import %s" import-name)
      (insert (format "import %s
" import-name))
      (beginning-of-line)
      (re-search-forward "^ *$")
      (setf import-end (point))

      (sort-lines nil import-start import-end))))

;; (defun python-kill-pylys ()
;;   "Sometimes you need to kill the pyls servers to get things working
;; again.

;;   It can be a bit touchy."
;;   (interactive)

;;   (dolist (proc-info (list-all-processes))
;;     (message "cmd: %s" (assoc1 'command proc-info))
;;     (when (string-match "pyls" (assoc1 'command proc-info))
;;       (message "kill %d" (assoc1 'pid proc-info))))
;;       ;; (do-cmd-async (list "kill" (number-to-string (assoc1 'pid proc-info))))
;;       )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code for parsing large blocks of formatted text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defun csv-split-text (text &key (split-regex ",") split-line-fn (has-header-line t) field-names)
  "Parse a csv file, and split it into a list of assoc lists, based on the csv header.

Note this is not all of csv, so it won't work on all files.  It's more for a quick
and dirty parsing of command output."
  (cl-flet ((--split-line (line)
              (cond
               (split-regex (string-split split-regex line))
               (split-line-fn (funcall split-line-fn line))
               (t (error "Must have either a split-regex or a split-line-fn defined")))))
    (let* ((lines (string->list text))
           (header-line (when has-header-line
                          (cl-first lines)))
           (lines (if has-header-line
                      (cdr lines)
                    lines))
           (field-names (if (and header-line
                                 (not field-names))
                            (--split-line header-line)
                          field-names)))
      (assert field-names nil "Error: must provide field-names")

      (mapcar (fn (line)
                (mapcar* #'cons
                         field-names
                         (--split-line line)))
              lines))))

;; (defun assoc-transpose (alist)
;;   )

(cl-defun assoc-to-csv (alist)
  "Convert an alist to csv."

  ;; figure out the headers
  (assert alist)

  (let* ((separator "\t")
         (headers (mapcar (| format "%s" %) (assoc-keys (cl-first alist))))
         (header-string (string-join headers separator)))
    (string-join
     (cons header-string
           (cl-loop for data in alist
                    collect (string-join (mapcar (| format "%s" %) (assoc-values data)) separator)))
     "\n")))

(defun ldif-unwrap-lines (lines)
  "Given a list of lines, unwrap any that start with a space, as in ldif:
https://www.ietf.org/rfc/rfc2849.txt."
  (nreverse (cl-reduce (fn (current next)
                         (let ((space-regex "^ +"))
                           (if (string-match space-regex next)
                               (progn
                                 (setcar current (concat (car current) (replace-regexp-in-string space-regex "" next)))
                                 current)
                             (cons next current))))
                       lines
                       :initial-value (list))))

(defun ldif-split-text (text)
  "Do a simple conversion of ldif to an assoc list."
  (cl-loop for line in (mapcar #'string-trim (ldif-unwrap-lines (split-string text "\n" t)))
           when (string-has-value-p line)
           collect (progn
                     (if-let (split (string-split ": " line))
                         (destructuring-bind (key value) split
                           (cons key value))
                       (debug)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Docker Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun docker-list-processes (&optional all)
  (csv-split-text (apply #'run-to-str "docker" "ps" `(,@(when all
                                                          (list "-a"))))
                  :split-regex "[ \t][ \t]+"))

(defun docker-list-images ()
  (csv-split-text (run-to-str "docker" "images") :split-regex "[ \t][ \t]+"))

(defun docker-list-processes-pp ()
  (interactive)
  (with-overwrite-buffer-pp "+docker-ps+"
    (docker-list-processes)))

(defun docker-list-container-ids ()
  (mapcar (| assoc1 "CONTAINER ID" %) (docker-list-processes)))

(defun docker-find-container-name (name)
  (filter (| equal name (assoc1 "NAMES" %))
          (docker-list-processes)))

(defun docker-find-started-container-name (name)
  (filter (| equal name (assoc1 "PORTS" %))
          (docker-list-processes t)))

(defun docker-start-container (container)
  (run "docker" "start" container))

(defun docker-container-started-p (name)
  (not (not (docker-find-started-container-name name))))

(defun docker-container-running-p (name)
  (not (not (docker-find-container-name name))))

(defalias 'docker-ps #'docker-list-processes-pp)

(defun docker-list-images-pp ()
  (interactive)
  (with-overwrite-buffer-pp "+docker-images+"
    (docker-list-images)))

(defun docker--arg-join (arg0name alist separator)
  (string-join (mapcar (fn ((key . value))
                         (format "%s %s%s%s" arg-name
                                 (shell-quote-argument key)
                                 separator
                                 (shell-quote-argument value)))
                       alist)

               " "))

(defun docker-volume-join (volumes-alist)
  (docker--arg-join "--volume" volumes-alist ":"))

(defun docker--env-vars-join (env-vars)
  (docker--arg-join "--env" env-vars "="))

(cl-defun docker-run-image (image-id &key name env-vars file-map)
  "Run a docker image denoted by `IMAGE-ID'.

Also support naming the image for future reference with `NAME'.

`ENV-VARS': an alist of any environment variables you want set.
`FILE-MAP': An alist of local -> image file mounts."
  (with-shell-buffer "~" (format "*docker-container-%s-%s-docker*" image-id (or name ""))
    (insertf "# To install bash in Apline, do: apk add bash\n")
    (insertf "docker run %s %s %s -it %s /bin/sh"
             (when name
               (format "--name %s" name))
             (docker--env-vars-join env-vars)
             (docker--volume-join file-map)
             image-id)
    (comint-send-input nil t)))

(defun docker-exec-to-container (container-id)
  (with-shell-buffer "~" (format "*docker-container-%s-docker*" container-id)
    (insertf "# To install bash in Apline do: apk add bash\n")
    (insertf "# To isntall stuff in aws use yum\n")
    (comint-send-input nil t)

    (insertf-send "docker exec -it %s /bin/sh" container-id)))

(defun docker-connect (container-id)
  "Note that this is redundant of what you can do with tramp directly, but it helps me remember."
  (interactive (list (completing-read "container-id: " (docker-list-container-ids) nil t)))

  (find-file "/docker:%s:/" container-id))

;;
;; Should add some support for port mapping:
;; docker -p <exterior:interior>
;;
(defun docker-run-image-at-point ()
  (interactive)

  (docker-run-image (find-thing-at-point "[ \t\n]" :thing-name "Docker Image")))

(defun docker-run-latest-image ()
  (interactive)
  (if-let (image-id (assoc1 "IMAGE ID" (car (docker-list-images))))
      (docker-run-image image-id)
    (error "Could not find an image to run")))

;; (defun docker-connect-to-image-at-point ()
;;   (interactive)

;;   (docker-run-image-at-point))


(defun docker-image-prune ()
  (interactive)
  (run-async (list "docker" "image" "prune")))

(defun docker-stop (container-id)
  (run-async (list "docker" "stop" container-id)))

(defun docker-stop-at-point ()
  (interactive)

  (docker-stop (find-thing-at-point "[^a-z0-9]")))

;  docker image prune
;;
;; You can use tramp to edit stuff inside of docker:
;; https://willschenk.com/articles/2020/tramp_tricks/
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Template commands.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Other enhancements:
;; 1. make it recursive (you can import other templates and have
;;    them filled.)
;; 2. fstring like feature?
;; 3. refer to function calls?
;;
;; Could also integrate with python to access jinja2 templates.
;;

(defun string-template-fill (template vars)
  "A simple template filling function.  Something like this probably exists
   already.

   Variables are: ${[a-z0-9A-Z_]+}."
  (cl-flet ((sub-fn (matched-var)
                    ;;
                    ;; this seems stupid, but I haven't found a better
                    ;; way to do it.
                    ;; It seems like you should get the match from
                    ;; match-string-no-properties, but that doesn't
                    ;; seem to register tha match made by string-replace.
                    ;;
                    (let ((key (cl-first (string-find "${\\([^}]+\\)}" matched-var))))
                      (assert key)
                      (if-let (value (or (assoc-get key vars)
                                         (assoc-get (intern key) vars)))
                          value
                        (error "Unable to find substitute for key %s" key)))))
    (cl-loop for (k . v) in vars
             do (setf template (replace-regexp-in-string "${[a-zA-Z0-9_:]+}" #'sub-fn template t)))
    template))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-in-jira-at-point-internal (url)
  "This is meant to be wrapped by another function.

   It takes the current point and opens a url with it
   as if it was a jira ticket."

  (browse-url (path-join url "browse" (find-thing-at-point "[^A-Za-z0-9-]" :thing-name "Jira Ticket"))))

(defun insert-date ()
  (interactive)
  (insert (format-time-string "%c")))

(defun random-items (n list)
  "Pick a random sample of `n' items from a `list', using Reservoir Sampling.

See: https://en.wikipedia.org/wiki/Reservoir_sampling
 "
  (let ((output (ht)))
    (cl-loop for item in list
             for i from 0
             if (< (ht-size output) n)
                do (progn
                     (ht-set! output i item))
             else
                do (when (< (random* 1.0) (/ (float n) (float (+ i 1))))
                     ;; discard a saved item.
                     (ht-set! output (random n) item)))
    (ht-values output)))

(defun random-choice (list)
  "Select a random item from the list."
  (cl-first (random-items 1 list)))

(defun random-words (n &optional max-word-len)
  "Return `N' random words from the dictionary"
  (random-items n
                (filter (if max-word-len
                            (lambda (word) (<= (length word) max-word-len))
                          #'identity)
                (string->list (slurp "/usr/share/dict/web2")))))

(defun remove-newlines (begin end)
  (interactive "r")
  (replace-regex-region "[\n ]+" " " begin end t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; web-request functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defconst +firefox-dir-osx+ (expand-file-name "~/Library/Application Support/Firefox/"))
(defconst +firefox-profile-dir-osx+ (path-join +firefox-dir-osx+ "Profiles"))
(defconst +firefox-profiles-ini+ (path-join +firefox-dir-osx+ "profiles.ini"))

(defconst +chrome-dir-osx+ (expand-file-name "~/Library/Application Support/Firefox/"))
(defconst +chrome-profile-dir-osx+ +chrome-dir-osx+)

(defun firefox-get-default-profile-osx ()
  (when-let (default-profile (first (filter (fn (entry)
                                              (and (string-starts-with "Profile" (car entry))
                                                   (equal "1" (assoc-get "Default" entry ""))))
                                            (ini-load-file +firefox-profiles-ini+))))
    (basename (assoc1 "Path" default-profile))))

(defun firefox-extract-all-cookies (profile-name)
  (let* ((cookie-db (sqlite-open (expand-file-name (path-join +firefox-profile-dir-osx+ profile-name "cookies.sqlite"))))
         (param-list (cl-loop for column in '("host" "name" "value" "path"
                                              "originAttributes" "expiry" "isSecure" "isHttpOnly")
                              for i from 0
                              collect (cons column i)))
         (select-names (string-join (mapcar #'car param-list) ", ")))

    (sqlite-select cookie-db (format "SELECT %s FROM moz_cookies" select-names) nil 'full)))

(defun firefox-select-cookies (hosts &optional profile-name)
  (let* ((profile-name (or profile-name
                           (firefox-get-default-profile-osx)))
         (cookie-db (sqlite-open (expand-file-name (path-join +firefox-profile-dir-osx+ profile-name "cookies.sqlite"))))
         (param-list (cl-loop for column in '("name" "value")
                              for i from 0
                              collect (cons column i)))
         (select-names (string-join (mapcar #'car param-list) ", ")))

    (cl-loop for cookie in (sqlite-select cookie-db (format "SELECT %s FROM moz_cookies WHERE host in (%s)"
                                                            select-names
                                                            (string-join (repeat-elm (length hosts) "?") ", "))
                                          hosts)
             collect (cons (elt cookie (assoc1 "name" param-list))
                           (elt cookie (assoc1 "value" param-list))))))

;; get chrome db version:
;; version < 24 don't have the domain prefix
;;

(defun chrome-get-cookie-password-osx ()
  (osx-security-get-cached-password "Chrome Safe Storage" "Chrome"))



(defun web-cookies--url-to-cookie-hosts (url)
  (let ((parts (string-split "\\." (url-host (url-generic-parse-url url)))))
    (mapcar (| string-join % ".")
            (cl-loop for p = parts then (rest p)
                     while (>= (length p) 2)
                     collect (cons "" p)
                     collect p))))

(defun firefox-find-cookies-for-url (url)
  (firefox-select-cookies
   (web-cookies--url-to-cookie-hosts url)))

(defun chrome-extract-cookies ()
  (let ((cookie-db (path-join +chrome-profile-dir-osx+ "Default" "Cookies")))
    cookie-db
  ))

(defun url-encode-params (params)
  "Doc encode an alist into url parameters.  Nil implies a single param."
  (string-join
   (loop for (k . v) in params
         collect (format "%s=%s"
                         (url-hexify-string (format "%s" k))
                         (if v
                             (url-hexify-string (format "%s" v))
                           "")))
   "&"))

(defun url-build (url params)
  "Construct a url from its base path parts and the parameters.

Be sure to url encode the parameters.

`params' should be an alist of key value pairs."
  (if params
      (concat url "?" (url-encode-params params))
    url))

(defun parse-http-header (header)
  "A simple header splitter."
  ;; see:
  ;; http://www.bizcoder.com/everything-you-need-to-know-about-http-header-syntax-but-were-afraid-to-ask
  ;; for more details
  (if-let (result (string-find "\\([^:]+\\)[ \t]*:[ \t]*\\(.*\\)$"
                               (string-trim-right header)))
      (cons (string-to-keyword (downcase (cl-first result)))
            (second result))
    (error "Unable to parse http header: %s" header)))

;;
;; HTTP/1.0 100 Continue
;; HTTP/1.1 200 OK
;; HTTP/1.1 401
;;
(defun parse-http-status-line (line)
  ;; Could this be replaced with something like (string-find-named)
  (if-let (result (string-find "^HTTP/\\([0-9.]+\\) \\([0-9]+\\)\\(.*\\)$" line))
      (destructuring-bind (version code message) result
        `((:http-version . ,version)
          (:status-code . ,(string-to-number code))
          (:message . ,(string-trim message))))
    (error "Unable to parse http status line: %s" line)))

(defun coalesce-alist (alist)
  (let ((output))
    (dolist (entry alist)
      (destructuring-bind (k . v) entry
        (if (assoc k output)
            (setcdr (assoc k output)
                  (concatenate 'list (to-list (assoc1 k output)) (list v)))
          (push entry output))))
    (nreverse output)))

(defun stream-vec (vec)
  (let ((pos 0))
    (fn (op)
      (cl-case op
        (:next (when (< pos (length vec))
                 (prog1
                     (aref vec pos)
                   (cl-incf pos))))
        (:peek (aref vec pos))
        ;; don't add unless we need it.
        ;; (:prev (when (> pos 0) (decf pos)))
        ;; (:pos pos)
        (:empty-p (= pos (length vec)))))))

(defun stream-list (lst)
  (let ((head lst))
    (fn (op)
      (cl-case op
        (:next (prog1
                   (car head)
                 (setf head (cdr head))))
        (:peek (car head))
        (:empty-p (null head))))))

(defun parse-http-header-block (curl-header-block)
  "Parse a response header block from curl\'s stderr"
  (let ((status-line)
        (headers)
        (found-real-status))
    (dolist (line (mapcar (| string-left-trim-regex "< *" %)
                          (cl-remove-if-not (| string-starts-with "<" %)
                                            (string->list curl-header-block "\r*\n+"))))
      (let* ((line-info (or (when-let (parsed (ignore-errors (parse-http-header line)))
                              (cons 'header parsed))
                            (when-let (parsed (ignore-errors (parse-http-status-line line)))
                              (cons 'status parsed))))
             (line-type (car line-info))
             (parsed-line (cdr line-info)))

        ;; idea: once you find a real header,
        ;; the last status line is the one you go with.
        (cond
         ((and (eql line-type 'header) found-real-status)
          (push parsed-line headers))
         ((eql line-type 'status)
          ;;
          ;; Assume that once we get through the redirects, the first status is
          ;; the start of something legit.
          ;;
          (unless (or (http-code-is-informational-p (assoc1 :status-code parsed-line))
                      (http-code-is-redirect-p (assoc1 :status-code parsed-line)))
            (assert (not headers) t "You shouldn't have a status line after you start looking at headers")
            (setf status-line parsed-line)
            (setf found-real-status t))))))
    `((:status-line . ,status-line)
      (:headers . ,(coalesce-alist headers)))))

(defun parse-html-string (str &rest parse-args)
  (with-temp-buffer
    (insert str)
    (apply #'libxml-parse-html-region (point-min) (point-max)
           parse-args)))

(defun web-request--alist-to-cookies (alist)
  (string-join
   (mapcar (fn ((key . value))
             (format "%s=%s" key value))
           alist)
   "; "))

(defun web-request--handle-auth (auth)
  (if (cl-search ":" auth)
      auth
    (concat auth ":" (read-user-password (format "Password for (%s): " auth) auth))))

(defconst *webrequest-http-error-msg-len* 256)

(defun http-classify-status-code (code)
  (cond
   ((< code 0) :unknown-error)
   ((<= 100 code 199) :informational)
   ((<= 200 code 299) :success)
   ((<= 300 code 399) :redirect)
   ((<= 400 code 499) :client-error)
   ((<= 500 code 599) :server-error)
   (t :unknown-error)))

(defun http-code-is-redirect-p (code)
  "Return true if `CODE' represents an http redirect"
  (eql :redirect (http-classify-status-code code)))

(defun http-code-is-informational-p (code)
  "Return t if `CODE' represents an http redirect"
  (eql :informational (http-classify-status-code code)))

(defun content-type-html-p (header)
  (not (not (cl-search "text/html" (downcase header)))))

(defconst web-request-shell-script-template "#!/bin/bash -xe

ATTACHMENT=request-attachment.json
echo '%s' > ${ATTACHMENT}

# Command
%s

rm -f ${ATTACHMENT}

")

;; web-request dynamic variables.
(defvar *web-request-preserve-request* nil "Turn the web-request into a script in addition to sending it.")
(defvar *web-request-cache-urls* nil "Use the url-cache to save requests if possible.  Is a ttl-sec value.")
(defvar *web-request-debug-request* nil "Enter the debugger before the request is sent.")
(defvar *web-request-proxy-url* nil "The url of the proxy to use for making web requests.")
(defvar *web-request-proxy-auth* nil "The auth to use with the proxy to use for making web requests.")
(defvar *web-request-keep-directory* nil "Don't delete the temp directory web request uses.")

(let ((web-request-async-counter (make-counter)))
  (defun next-web-request-id ()
    "Return the next command id `web-request' should use for logging."
    (funcall web-request-async-counter)))

;;
;; Stuff to support
;;
;; 1. Create a 'map-fn' argument that applies the function to the output
;;    befor handing it back to either the callback-fn or returning it.
;;

(cl-defun web-request (url
                       &key
                       (method "GET")
                       params
                       auth
                       proxy-auth
                       bearer-auth
                       token-auth
                       body
                       json
                       data
                       form
                       file
                       headers
                       headers-secret
                       content-type
                       no-redirect
                       timeout
                       insecure
                       user-agent
                       cookie-jar
                       cookies
                       use-firefox-cookies
                       output-file
                       referer
                       retry
                       retry-delay-sec
                       throw
                       upload-file
                       map-fn
                       async
                       callback-fn
                       proxy)
  "Make a web request with curl.

   Params:
   `URL': The url to fetch

   Optional Params:
   `METHOD': Which http operation/method to perform, (defaults to GET).
   `PARAMS': An alist of url parameters.  A nil value means only send the key
   `AUTH': Auth can be: a user name with no colon, which will trigger a prompt for
           a password, or a full curl auth string like \"user:password\".
   `PROXY-AUTH': Can be: a user name with no colon, which will trigger a prompt for
                 a password, or a full curl auth string like \"user:password\".
   `BEARER-AUTH': An api token string for use in the Authorization header as Bearer.
   `TOKEN-AUTH': An api token string, for use in the Authorization header as Token.
   `BODY': A string that will be sent as a data body to the server.  Uses --data-raw.
   `JSON': An alist that will be converted to json and sent to the server.
   `DATA': An alist that will be url-encoded and sent to the server in a request body.
   `FORM': An alist that will be sent as a multi-part form body.
   `HEADERS': An alist of headers to override any headers to send on the request.  See *man curl* for rules.
   `CONTENT-TYPE': A string, set the content-type header to this value.
   `HEADERS-SECRET': An alist of headers you don't want shown in the *messages* buffer.
   `NO-REDIRECT': Don't follow redirects for this request.
   `FILE': A file to upload to the server.
   `TIMEOUT': A time in seconds to wait for the request to finish before giving up.
   `INSECURE': Don't verify ssl certificates.  (only use if you know what you're doing.)
   `USER-AGENT': Set the user agent string.
   `COOKIE-JAR': A place to send and receive cookies.
   `COOKIES': An alist of cookies to send.
   `USE-FIREFOX-COOKIES': Try to read cookies from Firefox and send them with a request.
   `OUTPUT-FILE': Dump the output to this file instead of stdout.
   `REFERER': Set the referer header for the request.
   `RETRY': Number of times to attempt the command
   `RETRY-DELAY-SEC': Time to wait betwey retries in seconds.
   `THROW': If `T' raise an error when something goes wrong, otherwise just return
            the error code.
   `UPLOAD-FILE': The path to a file to upload to the target.
   `MAP-FN': Apply the function to the data before handing back to the callback.
   `ASYNC': Don't block, but also don't worry about calling a callback.
   `CALLBACK-FN': Don't return anything, and call the callback-fn when the result returns.
   `PROXY': Provide a url as an argument to --proxy.

   Dynamic Global Variables
   `*WEB-REQUEST-PRESERVE-REQUEST*': Instead of making the request, dump everything to a script to run for
                         testing.
   `*WEB-REQUEST-DEBUG-REQUEST*': Enter the debugger before sending the request.
   `*WEB-REQUEST-CACHE-URLS*': Use the url cache to save lookup time.  Can be nil, :forever, or a timeout in sec.
   `*WEB-REQUEST-PROXY-URL*': Provide the url as an argument to --proxy.  Can also be a function which will
                  be called with the url as its argument.
   `*WEB-REQUEST-PROXY-AUTH*': Provide auth for a proxy, a la proxy user.  Can also be a function.

   Returns:
   An alist with the following information:
   `:RESP': The unparse response text from the server.
   `:CODE': The return code from calling curl. 0 is success.
   `:HTTP-CODE': The http code returned by the request, if available.
   `:HEADERS': An alist of the http headers.  Duplicated headers are coalesced into
               list.
   `:STDERR': Anything curl returns on stderr.
   `:JSON': An alist representing any JSON returned by the server.
   `:HTML': The parsed version of an html page."

  ;; Check the args
  (assert url)
  (let ((bjff (mapcar (fn (elm) (if elm 1 0))
                     (list body data json form file))))
    (assert (<= (sum bjff) 1) nil
            (format "You must only chose one of :body, :data, :json, :file, or :form: %s" bjff)))
  (if timeout
      (assert (integerp timeout)))

  ;; I need to move the web-request stuff its own module
  (when *web-request-cache-urls*
    (require 'm-url-cache)
    (m-url-cache-init))

  ;; Maybe combine these two
  (when token-auth
    (assert (stringp token-auth))
    (assert (not (assoc-get "Authorization" headers-secret)))
    (push (cons "Authorization" (format "Token %s" token-auth))
          headers-secret))

  (when bearer-auth
    (assert (stringp bearer-auth))
    (assert (not (assoc-get "Authorization" headers-secret)))
    (push (cons "Authorization" (format "Bearer %s" bearer-auth))
          headers-secret))

  (when use-firefox-cookies
    (unless user-agent
      (setf user-agent "Mozilla/5.0 (platform; rv:gecko-version) Gecko/gecko-trail Firefox/firefox-version"))
    (setf cookies (concatenate 'list (firefox-find-cookies-for-url url) cookies)))

  ;; Is there a better way to do this?
  (when cookies
    (when-let (cookie-str (web-request--alist-to-cookies cookies))
      (push (cons "Cookie" cookie-str)
            headers-secret)))

  ;; Build up the command list.  Use a tmpdir to
  ;; cleanup any files created.
  (with-tempdir (:root-dir "/tmp" :leave-dir t)
    (let* ((cmd (list "curl" "--verbose" "--silent"))
           (json-file "request-attachment.json")
           (data-file "url-encoded-data.txt")
           (callback-fn (or callback-fn  (when async #'identity)))
           (proxy-auth (or proxy-auth *web-request-proxy-auth*))
           (web-request-id (next-web-request-id))
           (web-temp-dir default-directory)
           ;;
           ;; I should probably clean this up...
           ;; consolidate quoting and how we create values,
           ;; etc.  Make it its own function
           ;;
           (input-file (when (or auth proxy-auth headers-secret)
                         (let ((input-file-path "input-file"))
                           (touch input-file-path)
                           (chmod input-file-path #o600)
                           ;; TODO: There's probably a nicer way to do this.
                           (barf (string-join (concatenate 'list
                                                           (when auth
                                                             (list (format "user = %s" (quote-str (web-request--handle-auth auth)))))
                                                           (when proxy-auth
                                                             (list (format "proxy-user = %s" (quote-str (web-request--handle-auth proxy-auth)))))
                                                           (cl-loop for (name . value) in headers-secret
                                                                    collect (format "header = %s" (quote-str
                                                                                                   (concat name ": " value)))))
                                              "\n")
                                 input-file-path)
                           input-file-path)))
           (final-url (url-build url params))
           (use-caching *web-request-cache-urls*))

      (message "Running in tempdir: %s" web-temp-dir)

      ;; Dump out the json to a file, if needed.
      (when json
        (barf (if (typep json 'string)
                  json
                (json-encode json))
              json-file))

      ;; Dump url-encoded data, if needed.
      (when data
        (barf (url-encode-params data) data-file))

      (cl-flet ((append-option (arg value-fn)
                  (when arg
                    (setf cmd (append cmd (funcall value-fn))))))

        ;; TODO(mls): do I need append option, now that I have append!?
        ;; I guess the check for validity is nice.
        (unless no-redirect
          (append-atom! cmd "-L"))
        (append-option method (| (list (upcase (format "-X%s" method)))))
        (append-option input-file (| '("-K" "-")))
        (append-option retry (| (list "--retry" (number-to-string retry))))
        (append-option retry-delay-sec (| (list "--retry-delay" (number-to-string retry-delay-sec))))
        (append-option user-agent (| `("-A" ,user-agent)))
        (append-option cookie-jar (| `("--cookie" ,cookie-jar "--cookie-jar" ,cookie-jar)))
        (append-option insecure (| `("--insecure")))
        (append-option output-file (| list "--output" output-file))
        (append-option referer (| list "--referer" referer))
        (append-option upload-file (| list "--upload-file" upload-file))

        ;; https://gist.github.com/joyrexus/524c7e811e4abf9afe56
        (when form
          (cl-loop for (name . value) in form
                   for i from 0
                   do
            (let ((filename (format "form-file-%s" i)))
              ;; Write the value out to a file, just so we don't
              ;; overwhelm the arg list.
              (cond
               ((atom value) (barf value filename))
               ((equal :file (cl-first value))
                (setf filename (second value)))
               (t (error "Invalid form value: %s" value)))

              (append! cmd (list "-F" (format "%s=@%s" name filename))))))
        (append-option body (| `("--data-raw" ,body)))
        (cl-loop for (name . value) in headers do
                 (append-option t (| `("-H" ,(format "%s: %s" name value)))))

        ;; Is there a nicer way to do this?
        (when (or json data content-type)
          (append-option t
                         (fn ()
                           (list "-H"
                                 (format "Content-Type:%s"
                                         (or content-type
                                             (if json
                                                 "application/json"
                                               "application/x-www-form-urlencoded")))))))
        (append-option json (| `("--data" ,(concat "@" json-file))))
        (append-option data (| `("--data" ,(concat "@" data-file))))
        (append-option file (| `("--data-binary" ,(concat "@" file))))
        (append-option timeout (| `("--connect-timeout" ,(format "%d" timeout))))
        (append-option t (| list final-url ))

        (let ((proxy-url (or proxy *web-request-proxy-url*)))
          (append-option proxy-url (| `("--proxy" ,proxy-url))))

        (when *web-request-preserve-request*
          (let ((output-file (path-join "~" (concat (url-hexify-string url) ".sh"))))
            (message "web-req[%d]: Preserve request here: %s" web-request-id output-file)

            (barf (format web-request-shell-script-template
                          (slurp json-file)
                          (string-join cmd " "))
                  output-file)
            (chmod output-file #o700)))
        (when *web-request-debug-request*
          (debug))

        ;;
        ;; This gets complicated because we've elected to support async, sync, and caching
        ;; in the same function.
        ;;
        ;; use cl-labels to break shared code in separate functions, so I can keep the
        ;; 'if' complexity to a minimium.
        ;;
        (cl-labels ((is-async ()
                      "Is this call async?  This is mostly for future proofing."
                      (not (not callback-fn)))
                    (handle-response-fn (resp)
                      "Handle a `resp' response structure either from the url cache or from do-cmd/do-cmd-async"

                      ;;
                      ;; Cleanup the tempdir now that the requestion should be finished.
                      ;;
                      (when (and (file-exists-p web-temp-dir)
                                 (not *web-request-keep-directory*))
                        (message "Deleting web temp directory: %s" web-temp-dir)
                        (delete-directory web-temp-dir t))

                      ;; Try to share the cache updating code between sync and async mode
                      (when use-caching
                        (message "web-req[%d]: handle-response cache url %s code %s" web-request-id final-url
                                 (assoc1 :code resp))
                        (m-url-cache-set final-url resp
                                         (if (equal use-caching :forever)
                                             nil
                                           use-caching)))

                      (let* ((curl-output (assoc1 :stdout resp))
                             ;; Split out the '< content-type: application/json' headers
                             ;; from curl, and turn them into an alist.
                             (resp-block (parse-http-header-block (assoc1 :stderr resp)))
                             (status-line (assoc1 :status-line resp-block))
                             (curl-code (assoc1 :code resp))
                             (http-code (if (and (= curl-code 0) status-line)
                                            (assoc1 :status-code status-line)
                                          -1))
                             (http-error-type (http-classify-status-code http-code))
                             (headers (assoc1 :headers resp-block))
                             ;; I decided to always try to parse json, if only
                             ;; because sometimes I deal with improperly implemented
                             ;; web services.
                             (resp-json (ignore-errors
                                          (json-read-from-string (assoc1 :stdout resp))))
                             (resp-html (when (content-type-html-p (assoc-get :content-type headers ""))
                                          (ignore-errors
                                            (parse-html-string curl-output)))))

                        (message "web-req[%d]: finished: curl code: %s http code: %s"
                                 web-request-id curl-code http-code)

                        ;;
                        ;; Check to see if we're supposed to throw an error
                        ;;
                        ;;
                        (when throw
                          (unless (eq http-error-type :success)
                            (let ((error-message
                                   (format "HTTP Request Failed.  Code: %s, Status: (%s), Text: (%s)"
                                           http-code (assoc1 :message status-line)
                                           (string-truncate curl-output *webrequest-http-error-msg-len*))))
                              (message "web-req[%d]: %s" web-request-id error-message)
                              (error error-message))))

                        ;; If the alist gets so large it's annoying in ielm,
                        ;; I could make separate functions for accessing
                        ;; json or parsed html.
                        (let* ((output `((:http-code . ,http-code)
                                         (:http-status . ,http-error-type)
                                         (:status-line . ,status-line)
                                         (:headers . ,headers)
                                         (:final-url . ,final-url)
                                         (:method . ,method)
                                         (:url . ,url)
                                         (:params . ,params)
                                         (:curl-code . ,curl-code)
                                         (:stderr . ,(when curl-code
                                                       (assoc1 :stderr resp)))
                                         (:resp . ,curl-output)
                                         (:output-file . ,output-file)
                                         (:json . ,resp-json)
                                         (:html . ,resp-html)))
                               (output (if map-fn
                                           (funcall map-fn output)
                                         output)))
                          (if (is-async)
                              (funcall callback-fn output)
                            output))))
                    (make-request-fn ()
                      (let ((args (list :input input-file
                                        :stdout 'string :stderr 'string :throw throw)))
                        (if (is-async)
                            (apply #'do-cmd-async cmd :callback-fn #'handle-response-fn args)
                          (handle-response-fn (apply #'do-cmd cmd args))))))

          ;;
          ;; Actually make the web request, either async or not
          ;; caching the output if needed.
          ;;
          (message "web-req[%d]: start %s request for %s" web-request-id method final-url)

          (if use-caching
              (if-let (resp (m-url-cache-get final-url))
                  ;; We already have the cached response from the url cache.
                  (progn
                    (message "web-req[%d]: cache hit on %s" web-request-id final-url)
                    (handle-response-fn resp))
                (make-request-fn))
            (make-request-fn)))))))

;; TODO(mls): make this consistent with do-cmd-succeeded-p
(defun web-request-success-p (resp)
  (eq :success (assoc1 :http-status resp)))

(defun web-request-for-status (&rest args)
  (web-request-success-p (apply #'web-request args)))

(defun web-request-reset-proxy-password (proxy-user)
  "If you need remove the cached proxy password for `web-request' run this."
  (password-cache-remove proxy-user))

(defun web-request-copy-file (file target-url &optional auth)
  "Use web-request/curl to copy a file to a remote host."
  (web-request target-url :upload-file file
               :throw t
               :auth auth))

(cl-defun download-file (url &key (target-dir (expand-file-name "~/Downloads")) cb-fn name)
  (let ((target-file (or name (url-basename url))))
    (cl-assert (not (string-nil-or-empty-p target-file))
               "Invalid target file")

    (let ((output-file-path (path-join target-dir target-file)))
      (web-request url
                   :output-file output-file-path
                   :throw t
                   :callback-fn (lambda (resp)
                                  (when cb-fn
                                    (message "Downloaded file %s" output-file-path)
                                    (funcall cb-fn output-file-path))))
      output-file-path)))

(defun normalize-dir-path (path)
  (concat (string-remove-suffix "/" (expand-file-name path)) "/"))

(defun enumerate (seq)
  (cl-loop for elm in seq
           for x from 0
           collect (cons x elm)))

(defun open-custom-terminal (input-script name)
  "Open a terminal and insert `input-script'.

   Also give the terminal the name `name'."

  (let ((buffer (generate-new-buffer (format "*term-%s-term*" name))))
    (with-current-buffer buffer
      (term-mode)
      (term-exec buffer "terminal" "/bin/bash" nil nil)
      (insertf "%s" input-script)
      (term-char-mode))
    (switch-to-buffer buffer)
    buffer))

(defun host-info-to-login (host-or-ip &optional user)
  (concat (if user (concat user "@") "")
          host-or-ip))

;; Should read ips from your ssh file.
(defun open-host (host-or-ip user)
  (interactive "shost-or-ip: \nsuser: ")
  (shell-open-dir (format "/ssh:%s:/" (host-info-to-login host-or-ip user))))

;; ssh -L [LOCAL_IP:]LOCAL_PORT:DESTINATION:DESTINATION_PORT [USER@]SSH_SERVER
(cl-defun ssh-open-tunnel (ssh-server local-port destination-port &key (destination-server "127.0.0.1") user local-ip)

  (let ((cmd "ssh -L "))
    (cl-labels ((force-str (atom)
                  (cl-case (type-of atom)
                    (integer (number-to-string atom))
                    (t atom)))
                (add-to-cmd (&rest strings)
                  (setf cmd (apply #'concat cmd (mapcar #'force-str strings)))))
      (when local-ip
        (add-to-cmd local-ip ":"))
      ;; This isn't accounting for just 2 unix domain sockets.
      (add-to-cmd local-port ":" destination-server ":" destination-port " ")

      (when user
        (add-to-cmd user "@"))
      (add-to-cmd ssh-server)

      ;; should I use split-string-and-unquote more?
      (shell-open-run-command "/tmp" (split-string-and-unquote cmd) "*ssh-tunnel-ssh*"))))

(defun tar-dir (output-file dir)
  (run "tar" "cfz" output-file dir))

;;
;; this is supposed to be for matching lists of alists.
;; I need to have a way to specify what data is returned.
;;
(defun assoc-query (query alist)
  (cl-labels ((match-query (query item)
                 (message "query: %s item: %s" query item)
                 (let ((cmd (car query))
                       (rest (cdr query)))
                   (message "cmd: %s rest: %s first rest %s" cmd rest (cl-first rest))
                   (ecase cmd
                     (and (and (mapcar (| match-query % item) rest)))
                     (or  (or  (mapcar (| match-query % item) rest)))
                     (not (not (match-query rest item)))
                     (key (assoc (cl-first rest) item))
                     (value (rassoc (cl-first rest) item))))))

    (cl-first (cl-remove-if-not (| match-query query %) alist))))

(defun memoize (fn &optional timeout-sec)
  (let ((ht (ht))
                (fn fn)
                (timeout-sec timeout-sec))
    (fn (&rest args)
        (cl-labels ((lookup ()
                    (if-let (entry (ht-get ht args))
                        ;; Delete the current value if there is a timeout.
                        ;; car entry: value
                        ;; cdr entry: create time
                        (if (and timeout-sec
                                 (>= (time-to-seconds (time-subtract (current-time) (cdr entry))) timeout-sec))
                            (progn
                              (message "Memoize: Delete entry %s" args)
                              (ht-remove ht args)
                              (lookup))
                          (progn
                              (message "Memoize: Success.")
                              (car entry)))
                      (progn
                        (let ((res (apply fn args)))
                          (ht-set ht args (cons res (current-time)))
                          res)))))
          (lookup)))))

(defmacro memoize-fn (func &optional timeout-sec name)
  `(defalias ',(or name (symbol-rename func (| concat % "-cached")))
     (memoize #',func ,timeout-sec)))

;; TODO: Work on this.
;; It would be nice if it filled in the file names with
;; tab completion.
(defun tail-buffer (file)
  (interactive "sfile: ")
  (switch-to-buffer))

;; (defun export-org-table ()
;;   (interactive)
;;   (unless (org-at-table-p)
;;     (error "No table at the point."))

;;   (save-excursion
;;     (org-table-begin)
;;     )
;;   )

(defun search-http-repo (url match-fn)
  ;; Find all of the links in the page.
  (cl-loop for a-tag in (dom-by-tag (assoc1 :html (web-request url :throw t)) 'a)
           with href = nil
           do (setf href (assoc1 'href (dom-attributes a-tag)))
           when (funcall match-fn href)
           collect href))

(defmacro time-this-code (&rest body)
  "A timer function for functions."
  `(let ((time (current-time)))
     (unwind-protect
         (progn
           ,@body)
       (message "%.06f" (float-time (time-since time))))))

(defun current-clipboard ()
  "Return the current contents of the OS's clipboard."
  (with-temp-buffer
    (clipboard-yank)
    (buffer->string)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Firefox based commands.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst +osx-firefox-headless-profile "headless")

;; Note that these commands will shutdown a currently active firefox.
(defun firefox-cmd (cmd &rest args)
  (apply #'do-cmd-async (concatenate 'list
                                     (list +osx-path-to-firefox+ "-P" +osx-firefox-headless-profile)
                                     cmd)
         args))

(cl-defun firefox-screenshot (url &optional (output-dir (expand-file-name "~/Desktop/")))
  (pushd output-dir
    (firefox-cmd (list "--screenshot" url))))

;;
;; other interesting commands:
;; --search
;; --remote-debugging-port <port> Start the Firefox remote agent, which is
;; a low-level debugging interface based on the CDP protocol.
;; Defaults to listen on localhost:9222.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LDAP Lookup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ldap-query-to-string (query)
  (cond
   ((stringp query) query)
   ((listp query)
    (string-join
     (cl-loop for (key . val) in query
              collect (format "(%s=%s)" key val))
     ""))
   (t (error "The ldap query must be a list or a string."))))

(cl-defun ldapsearch (&key ldap-server search-base bind-dn passwd-fn query)
  "Use the ldapsearch command to bind to an ldap server and make a query.

  `ldap-server': The server host name.
  `search-base': The place in ldap to start looking, example: ou=users,dc=corp,dc=company,dc=com
  `bind-dn': Name to use for authenticating against the server
  `passwd-fn': Function will be called with no arguments.  It should return the bind-dn password.
  `query': an alist or string of query parameters."
  (let ((pw-file "pw.txt"))
    (with-tempdir (:root-dir "/tmp")
      (touch pw-file)
      (chmod pw-file #o600)
      (barf (funcall passwd-fn) pw-file)
      (ldif-split-text (run-to-str "ldapsearch" "-LLL"
                                   "-h" ldap-server
                                   "-b" search-base
                                   "-D" bind-dn
                                   "-y" pw-file
                                   (ldap-query-to-string query))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Security Functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I could move these to their own module at some point.

(defun osx-security-get-password (password-name user)
  "Use the osx 'security' command to get a password from the keychain.

   This may cause osx to prompt the caller to unlock the password.

   Args:
   `password-name': The name used when the password was created in the keychain.
   `user': the user id associated with the `password-name'.

   returns: the password string.
"
  ;; Don't use passwords
  (string-trim (run-to-str "security" "find-generic-password" "-w" "-s" password-name "-a" user)
               nil "\n"))

;; Can I unify this with `read-user-password'?
(defun osx-security-get-cached-password (password-name user)
  "Cache a password from the osx 'security' command.

   This may cause osx to prompt the caller to unlock the password.
   Once the caller has allowed access, emacs will store it in the password-cache
   for a user controlled amount of time.

   Args:
   `password-name': The name used when the password was created in the keychain.
   `user': the user id associated with the `password-name'.

   returns: the password string.
"
  (if (password-in-cache-p password-name)
      (password-read-from-cache password-name)
    (progn
      (let ((password (osx-security-get-password password-name user)))
        (password-cache-add password-name password)
        password))))

(defun common-lisp-insert-debug ()
  (interactive)
  ;;
  ;; Note: Try C-M-x function and it may put the code in its own window.
  ;; also C-u C-c C-c does the same thing
  ;;
  ;;
  ;; (declaim (optimize (debug 2)
  ;;          (sb-c::insert-step-conditions 0)
  ;;          (sb-c::insert-debug-catch 0)))
  ;;
  (insertf "(declaim (optimize (speed 0) (space 0) (debug 3)))"))

(defun list-all-defined-variables ()
  (cl-loop for x being the symbols
        if (boundp x)
        collect (symbol-name x)))

(defun dump-defined-variables ()
  (message "Variables: %s" (list-all-defined-variables)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Inferior Mode Handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common Lisp RPC with inferior lisp mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar +common-lisp-buffer+ "*inferior-lisp*")

(defun clrpc-send (form)
  (let ((proc (inferior-lisp-proc)))
    (comint-send-string proc (prin1-to-string form))
    (comint-send-string proc "\n")))

(provide 'elisp-lib)
