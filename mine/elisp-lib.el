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
(require 'subr-x)
(require 'json)
;; (require 's)
;; (require 'dash)
;; (require 'ht)

;; Wrap in try catch
(setf lexical-binding t)

(setf directory-sep "/")

(defun assoc1 (keys list)
  "Lookup a key in an alist and raise an error if its not there.

   Returns the value (The cdr of the element).
   Values can be nil and it will still work.
   `key': The key to lookup
   `alist': The associated list to check.
  "
  (let ((keys (to-list keys)))
    (cl-flet ((as (key al)
                  (if-let (answer (assoc key al))
                      (cdr answer)
                    (error "Key \'%s\' not found" key))))

      (while keys
        (let ((res (as (car keys) list)))
          (setf list res)
          (setf keys (cdr keys)))))
    list))

(defun rassoc1 (value list)
  "Lookup a key in an alist and raise an error if its not there.

   Returns the value (The cdr of the element).
   Values can be nil and it will still work.
   `key`: The key to lookup
   `alist`: The associated list to check.
  "

  (if-let (answer (rassoc value list))
      (cdr answer)
    (error "Value \'%s\' not found" value)))

(defun assoc-keys (alist)
  "Return all of the keys in an alist.  That is the `car' values."
  (mapcar #'car alist))

(defun assoc-values (alist)
  "Return all of the values in an alist.  That is the `cdr' values."
  (mapcar #'cdr alist))

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
  (make-symbol (funcall cb (symbol-name sym))))

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
  (declare (indent 2))
  `(let ((,name ,test))
     (when ,name ,@body)))

(defmacro aif (test &rest forms)
  "Anaphoric if that sets the test value to _it_"
  (declare (indent 2))
  `(let ((_it_ ,test))
     (if _it_ ,@forms)))

(defmacro m-if-let (test-binding &rest forms)
  "Provides an if macro that binds a value a la let.
v
Example:
 (m-if-let (res (fetch-string))
   (convert-to-something res)
   (error \"failed to fetch string\"))
Here res is the name, and (fetch-string) is the
test clause.  If fetch-string returns nil, the
error case is executed, otherwise the success
case is executed.  res is bount to the result
of the test."
  (declare (indent 2))
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
  (declare (indent 2))
  (car
   (reduce (fn (prev-bindings binding)
               `((m-when-let ,binding
                   ,@prev-bindings)))
           (reverse test-bindings)
           :initial-value forms)))

(defun append-if (test list)
  (m-if-let (res test)
          (append list (to-list res))))

(defun cons-if (test list)
  (m-if-let (res test)
          (cons res list)))

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
  (declare (indent 2))

  `(let ((_it_ ,val))
     (if (,test-fn _it_) ,@forms)))

(defmacro if-not (test &rest forms)
  (declare (indent 2))

  `(if (not ,test) ,@forms))

(defun to-list (thing)
  "Convert thing to a list if it isnt already."
  (if (atom thing) (list thing) thing))

(defun last-car (list)
  (car (last list)))

(defun take (n list)
  (butlast list (- (length list) n)))

(defun drop (n list)
  (last list (- (length list) n)))

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
                         (t (m-if-let (old-sym (cdr (assoc arg-num alist-args)))
                                      (setf new-name old-sym)
                                      (setf alist-args (acons arg-num new-name alist-args)))))
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
  (declare (indent 2))
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
  "Generate a list of `n' `elm's."
  (cl-loop for x below n collect elm))

(defun repeat-string (n str)
  (let ((out ""))
    (cl-loop for x below n do
             (setf out (concat out str)))
    out))

(defun* partition (n list &key (step n) (pad nil))
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
  (let ((res (remove-if-not (| string-match regex %) list)))
    (if (string-has-val replace)
        (mapcar (| replace-regexp-in-string regex replace %) res)
      res)))

(defun all-true (list)
  ;; Note, since or is a macro, we can't do 'or
  (reduce (| and %1 %2) list))

(defun any-true (list)
  ;; Note, since or is a macro, we can't do 'or
  (reduce (| or %1 %2) list))

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
(defun string-split (sep-regex str)
  (split-string str sep-regex))

(defun string-nil-or-empty (s)
  (= 0 (length s)))

(defun string-has-val (s)
  (not (string-nil-or-empty s)))

(defun string-ends-with (suffix str)
  (string-match (concat (regexp-quote suffix) "$") str))

(defun string-starts-with (prefix str)
  (string-match (concat "^" (regexp-quote prefix)) str))

(defun string-or (&rest args)
  ;; Loop through the args, return the first not nil or empty
  ;; string
  (dolist (str args) (if-not (string-nil-or-empty str) (return str))))

(defun string-trim-chars (str front-group back-group)
  "Remove leading and trailing characters from a string."
  (replace-regexp-in-string (format "\\(^[%s]+\\|[%s]+$\\)" front-group back-group) "" str))

(defun string-left-trim-regex (regex str)
  (replace-regexp-in-string (format "^%s" regex) "" str))

(defun string-right-trim-regex (regex str)
  (replace-regexp-in-string (format "%s$" regex) "" str))

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
  (declare (indent 2))
  `(if (string-has-val ,obj)
       ,@forms))

(defun replace-regex-region (regex replacement begin end)
  (save-excursion
    (goto-char begin)
    (while (re-search-forward regex end t)
      (replace-match replacement))))


;; TODO(scheinholtz): Do more with this?
(defalias 'string-replace 'replace-regexp-in-string)

(defalias 'string-replace-region 'replace-regex-region)

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

;; (defun dequote-str ()
;;   "Remove quotations ")


(defun string-case= (s1 s2)
  "Compare s1 and s2 ignoring case"
  (string= (downcase s1) (downcase s2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-fmt (fun str &rest args)
  (declare (indent 2))
  `(,fun (format ,str ,@args)))

;; TODO(scheinholtz): Use the async I/O for this.
(defun* doit-shell (doit-str silent fmt &rest args)
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

;; Design Notes:
;;
;; ideal interface:
;; (run-str "ls -l") -> "string of contents"
;; (run "ls" "-l" :stdout 'string :stderr "/dev/null")
;;
;; I may also want to add an 'environment' input to the function.
;; for stuff like ecr access.
;;
(cl-defun do-cmd (cmd &key input stdout stderr throw)
  ;; Add an async function
  "Be a main entry point for running shell commands.

  Wrapper around emacs's external shell command function (call-process),
  hopefully with a nicer interface.

  It executes the command and returns an alist with the results
  or any output if requested.

  The arguments are:
   1. cmd: a list of things to pass to bash -c.  The arguments shouldn't be
           interpreted by the shell.

   2. input: The file to hand to the input process.  nil is the same as dev

   3. stdout: What to do with the stdout from the subprocess.
             'current-buffer: Dump into the current buffer.
             'string:         Return it as a string in the result alist.
             <buffer object>
             nil
             (:buffer <buffer-name>)
             (:file <file-path 'append) ; append is optional.

   4. stderr: The same as stdout with one additional option.
              NOTE: this is on by default to make error messages better.
              'stdout: combine stdout and stderr.

   5. throw: Raise an error on failure instead of returning the alist.

  output: (list
           (:code . <int>) ; Did it succeed or fail
           (:stdout . \"string\")  ; if any
           (:stderr . \"string\")) ; if any
  "
  (let* ((program (first cmd))
         (arguments (cdr cmd))
         (stdout-buffer (when (equal stdout 'string)
                          (generate-new-buffer "*do-cmd-stdout*")))
         ;; We always save stderr.
         (stderr-file (make-temp-file "do-cmd-stderr"))
         (stdout (if stdout-buffer t stdout))
         (stderr (if stderr-file stderr-file stderr))

         ;; Add a section to remap stderr/stdout
         (stderr (ecase stderr
                   (stdout t)
                   (current-buffer t)
                   (otherwise stderr)))
         (stdout (ecase stdout
                   (current-buffer t)
                   (otherwise stdout)))
         (resp)
         (stderr-string))

    (cl-flet ((my-call-process ()
                (message "cmd: %s" (cmd-to-shell-string cmd))
                (setf resp (apply #'call-process program input (list stdout stderr) nil arguments))))
      (if stdout-buffer
          (with-current-buffer stdout-buffer
            (my-call-process))
        (my-call-process))

      (when (and stderr-file (file-exists-p stderr-file))
        (setq stderr-string (slurp stderr-file))
        (delete-file stderr-file))

      (if (and (not (equal resp 0))
               throw)
          (error "Command %s failed, code: %s, msg: \'%s\'" cmd resp (if stderr-string
                                                                         (string-trim (s-truncate 1024 stderr-string))
                                                                       "")))
      (let ((output '()))
        (push (cons :code resp) output)
        (when stdout-buffer
          (with-current-buffer stdout-buffer
            (push (cons :stdout (buffer-string)) output))
          (kill-buffer stdout-buffer))
        (when stderr-string
          (push (cons :stderr stderr-string) output))
        output))))

(defun run (&rest cmd-parts)
  "Execute a shell command given an argument list.  See `do-cmd'
   for return value.

   Example:   (run \"hdiutil\" \"attach\" path)"
  (do-cmd cmd-parts))

(defun run-to-str (&rest cmd-parts)
  "Run a command using `do-cmd' and return the result as a string.

   It will throw an error if the command fails."
  (assoc1 :stdout (do-cmd cmd-parts :stdout 'string :throw t)))

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

(defmacro with-new-buffer (name-prefix &rest body)
  (declare (indent 2))
  (let ((old-buffer (gensym))
        (new-buffer (gensym)))

    `(let* ((,old-buffer (current-buffer))
            (,new-buffer (generate-new-buffer ,name-prefix)))
     (switch-to-buffer ,new-buffer)
     (ignore-errors
       ,@body)
     (switch-to-buffer ,old-buffer))))


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

(defun get-chrome-path ()
  (ecase system-type
    (darwin "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
    (linux "google-chrome")))

(defun browse-url-chrome (url)
  (interactive "surl: ")
  (let ((browse-url-generic-program "/opt/google/chrome/google-chrome"))
    (browse-url-generic url)))

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

;; XXX Note! I haven't tested this one!
(defun browse-url-firefox-osx (url &optional new-window)
  "A `browse-url' function for firefox on OSX.  Normally we end up with
   whatever the 'open' command does, but for me I like when it opens a new
   window.  This function provides that feature.

   To enable it as the default browser do:
   (setq browse-url-browser-function 'browse-url-firefox-osx)
   (setq browse-url-new-window-flag t)"
  (interactive (browse-url-interactive-arg "URL: "))
  (do-cmd `("/Applications/Firefox.app/Contents/MacOS/Firefox"
            ,@(if new-window
                  (list "-new-window" url)
                (list url))))
  (run "open" "-a" "Firefox"))

(defun current-line ()
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
;;(defun list->info-tuple (list)
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
                         (assoc1 day-of-week week-days)))
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
    (if-let (idx (assoc1 elm time-map))
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
  (split-string path directory-sep t))

(defun path-join (&rest args)
  "Join a set of path parts together, removing any duplicate slashes (OSX/Unix only)."
  (cl-flet ((needs-slash (val check-for-slash)
               (when (and val (funcall check-for-slash directory-sep val))
                 directory-sep)))
    (let ((leading-slash (needs-slash (first args) #'string-starts-with))
          (trailing-slash (needs-slash (last-car args) #'string-ends-with)))
      (string-join (list leading-slash
                         (string-join (mapcar (| string-trim-chars %
                                                 directory-sep
                                                 directory-sep)
                                              args)
                                      directory-sep)
                         trailing-slash)))))

(defalias 'basename 'file-name-nondirectory)

(defun ensure-makedirs (path)
  (let ((dir (file-name-directory path)))
    (unless (file-exists-p path)
      (make-directory dir t))))

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

(defun directory-file-under-dir (dir file)
  "Is the `file' path underneath the directory `dir'?"
  (string-starts-with (file-truename dir) (file-truename file)))

(defun slurp (path)
  "Read the contents of the file specified by `path' into a string.

  Return that string to the caller, or throw an error on failure."

  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

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
                    (format " \"%s\""
                            (string-join
                             (->> arg-str
                                  (string-split " ")
                                  (mapcar (| format "s%s: " %)))
                             "\\n"))))
         (indent-region (line-beginning-position) (line-end-position)))
       (error "unable to parse arglist"))))

(defun set-default-directory (dir)
  (interactive "sdir: ")
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
(defmacro setup-jump-to-abbrev (abbrev-table)
  (let* ((table (eval abbrev-table))
         (abbreviations (mapcar #'car table))
         (abbrev (gensym)))
    `(progn
       (defun jump-to-shell (,abbrev)
         (interactive (list
                       (completing-read "abbrev: " ',abbreviations)))
         (jump-to-abbrev #'shell-open-dir ,abbrev ',table))
       (global-set-key "\C-xg" #'jump-to-shell)

       (defun jump-to-dired (,abbrev)
         (interactive (list
                       (completing-read "abbrev: " ',abbreviations)))
         (jump-to-abbrev #'dired ,abbrev ',table))
       (global-set-key "\C-x\C-g" #'jump-to-dired)

       (defun jump-to-magit (,abbrev)
         (interactive (list
                       (completing-read "abbrev: " ',abbreviations)))
         (jump-to-abbrev #'magit-status ,abbrev ',table))
       (global-set-key "\C-x\M-g" #'jump-to-magit)
       (defun yank-abbrev-path (,abbrev)
         (interactive (list
                       (completing-read "abbrev: " ',abbreviations)))
         (jump-to-abbrev #'kill-new ,abbrev ',table))
       (defalias 'yap #'yank-abbrev-path)

       ;; Should figure this out, but later:
       ;; (defun insert-abbrev-path (,abbrev)
       ;;   (interactive (list
       ;;                 (completing-read "abbrev: " ',abbreviations)))
       ;;   (jump-to-abbrev #'insert ,abbrev ',table))
       ;; (defalias 'iap #'insert-abbrev-path)
       )))

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
  "Add up numbers in a region of text."
  (interactive "r")
  (message "%d" (sum-col-region-fn begin end)))

(defmacro make-bookmark (name url &rest urls)
  `(defun ,name ()
     ,(format "The `%s' function opens up the %s url." name url)
     (interactive)
     (browse-url ,url t)
     ,@(mapcar (lambda (u)
                 `(browse-url ,u nil)) urls)))

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

(defun title-caps-to-underbar (begin end)
  (interactive "r")
  (let ((case-fold-search nil))
    (replace-string-in-region begin end
      (string-replace
        "\\([A-Z]\\)"
        (| concat "_" (downcase %))
        (buffer-substring-no-properties begin end)))))

;; Emacs' calculator might already have this.
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
             (if (string-ends-with "://" elm)
                 (concat trimmed "/")
               trimmed))))
    (string-join (mapcar #'clean-element args) "/")))

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

(defun osx-screen-lock ()
  "Lock the screen immediately"
  (interactive)
  (run "/System/Library/CoreServices/Menu Extras/User.menu/Contents/Resources/CGSession"
       "-suspend"))

(defun osx-screen-lock-later (mins)
  "Lock the screen after `mins' minutes."
  (interactive "nmins: ")
  (run-at-time (format "%s min" mins) nil #'osx-screen-lock))

(defun osx-sleep-now ()
  "Put the system to sleep immediately."
  (run "pmset" "sleepnow"))

(defun osx-sleep-soon (mins)
  (interactive "smins: ")
  (run-at-time (format "%s min" mins) nil #'osx-sleep-now))

(defmacro pushd (dir &rest body)
  "Run the body in this new default directory"
  (declare (indent 2))
  (let ((old-dir (gensym)))

    `(let ((,old-dir default-directory))
       (unwind-protect
           (progn
             (setf default-directory (if (file-name-absolute-p ,dir)
                                         ,dir
                                       (path-join ,old-dir ,dir)))
             ,@body)
         (setf default-directory ,old-dir)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun git-remote-origin-url ()
  (interactive)
  (if-let (url (string-trim (run-to-str "git" "config" "--get" "remote.origin.url")))
      url
    (error "Unable to find git remote.origin.url.  Is this a git repo?")))

(defun git-get-origin-info ()
  (destructuring-bind (host project repo)
      (string-find
       ;; It's matching "ssh://git@(git.tcc.li):7999/(oe)/(auth).git"
       "[[:word:]]+://[[:word:]]+@\\([[:alnum:]\.]+\\).*/\\([^/]+\\)/\\([^.]+\\).git"
       (git-remote-origin-url))
    (list host project repo)))

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

(defun git-init-repo (path)
  (run "git" "init" path))

(cl-defun git-commit-changes (path &key (message  "Save current files."))
  (pushd path
         (run "git" "add" ".")
         (run "git" "commit" "-a" "-m" message)))

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

    (let* ((python-NAME-regex "[A-Za-z_][A-Za-z0-9_]+")
           (python3-regex (list "python3"
                                "print("
                                (format "class\s+%s:" python-NAME-regex)
                                "nonlocal"
                                (format "{%s:\s*%s\s+for\s+"
                                        python-NAME-regex python-NAME-regex)))
           (python2-regex (list "xrange("
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
        (run-searches python3-regex (fn () (incf py3-score)))
        (run-searches python2-regex (fn () (decf py3-score))))

      ;; If the score is positive or zero, assume python3.
      (message "final score: %d" py3-score)
      (>= py3-score 0))))

(defun update-flymake-mask (file-pattern func)
  "Update the flyname mask for `file-pattern'.  This removes any previous matches"
  (setq flymake-allowed-file-name-masks
        (cons (list file-pattern func)
              (remove-if (| equal file-pattern (first %))
                         flymake-allowed-file-name-masks))))

(defun find-virtualenv-file (root-dir)
  "Search a Python repo for the most like venv \'activate\' file.

  `root-dir' is where to start the search."
  ;; Does activate exist somewhere.
  (car (first
        (sort (mapcar (fn (path)
                          (cons path (cond
                                      ((equal (normalize-dir-path (file-name-directory path))
                                              (normalize-dir-path root-dir)) 10)
                                      ((search "/target/" path) 5)
                                      (t 0))))
                      (directory-files-recursively root-dir "^activate$"))
              (fn (a b) (> (cdr a) (cdr b)))))))

(defun activate-virtualenv (location)
  (interactive (list (completing-read "location: " (mapcar #'first oe-project-table) nil t)))
  (insertf "source %s" (find-virtualenv-file (assoc1 location oe-project-table))))

(defun open-shell-dir-venv ()
  "Start a virtual env in the current repo."
  (interactive)
  (switch-to-buffer (shell-open-dir default-directory))

  (let* ((repo-root (git-project-root))
         (repo-name (basename repo-root))
         (activate-link (find-virtualenv-file repo-root)))
    (rename-buffer (generate-new-buffer-name (format "*venv-%s-%s-venv*" repo-name (buffer-name))))

    (message "Default directory: %s repo root: %s" default-directory repo-root)
    (assert activate-link)
    ;; Issue a command to setup the virualenv
    (goto-char (point-max))
    (insertf "source %s" activate-link)
    (comint-send-input nil t)
    (comint-clear-buffer)))

(defun find-in-jira-at-point-internal (url ticket-valid-fn)
  "This is meant to be wrapped by another function.

   It takes the current point and opens a url with it
   as if it was a jira ticket."

  (save-excursion
    (let* ((max-ticket-len 15)
           (negative-ticket-re "[^A-Za-z0-9-]")
           (end (1- (save-excursion (re-search-forward negative-ticket-re))))
           (start (1+ (save-excursion (re-search-backward negative-ticket-re))))
           (ticket (if (< start end)
                       (buffer-substring-no-properties start end)
                     nil)))
      (if (and ticket
               (< (- end start) max-ticket-len)
               (funcall ticket-valid-fn ticket))
          (browse-url (path-join url "browse" ticket))
        (error "Invalid JIRA ticket '%s' found from %s to %s" ticket start end)))))

(defun insert-date ()
  (interactive)
  (insert (format-time-string "%c")))

(defun random-choice (list)
  "Select a random item from the list."
  (nth (random (length list)) list))

(defun remove-newlines (begin end)
  (interactive "r")
  (replace-regex-region "[\n ]+" " " begin end))

;; Note that at some point we might need to support single arguments
;; (as in no equal)
(defun url-encode-params (params)
  "Doc encode an alist into url parameters."
  (string-join
   (loop for (k . v) in params
         collect (string-join (list
                               (url-hexify-string (format "%s" k))
                               (url-hexify-string (format "%s" v)))
                               "="))
   "&"))

(defun parse-http-header (header)
  "A simple header splitter."
  ;; see:
  ;; http://www.bizcoder.com/everything-you-need-to-know-about-http-header-syntax-but-were-afraid-to-ask
  ;; for more details
  (if-let (result (string-find "\\([^:]+\\)[ \t]*:[ \t]*\\(.+\\)$"
                               (string-trim-right header)))
      (apply #'cons result)
    (error "Unable to parse http header: %s" header)))

(defun coalesce-alist (alist)
  (let ((output))
    (dolist (entry alist)
      (destructuring-bind (k . v) entry
        (if (assoc k output)
            (setcdr (assoc k output)
                  (concatenate 'list (to-list (assoc1 k output)) (list v)))
          (push entry output))))
    (nreverse output)))

(defun parse-http-header-block (curl-header-block)
  "Parse a response header block from curl\'s stderr"

  (let ((seen-first-line nil)
        (resp-line)
        (headers))
    (dolist (line (remove-if #'string-empty-p
                             (mapcar (| string-left-trim-regex "< *" %)
                                     (remove-if-not (| string-starts-with "<" %)
                                                    (string->list curl-header-block "\r*\n+")))))
      (if seen-first-line
          (push (parse-http-header line) headers)
        (progn
          (setf resp-line (string-split " +" line))
          (setf seen-first-line t))))
    (list
     (cons :resp-line resp-line)
     (cons :headers (coalesce-alist headers)))))

(cl-defun web-request (url
                       &key op params auth body json file timeout insecure throw)
  "Make a web request with curl.

   Params:
   `url': The url to fetch

   Optional Params:
   `op': Which http operation to perform, (defaults to GET).
   `params': An alist of url parameters.
   `auth': An auth string suitable for passing to curl.  So \"user:password\".
   `body': A string that will be sent as a data body to the server.
   `json': An alist that will be converted to json and sent to the server.
   `file': A file to upload to the server.
   `timeout': A time in seconds to wait for the request to finish before giving up.
   `insecure': Don't verify ssl certificates.  (only use if you know what you're doing.)
   `throw': If `t' raise an error when something goes wrong, otherwise just return
            the error code.

   Returns:
   An alist with the following information:
   `:resp': The unparse response text from the server.
   `:code': The return code from calling curl. 0 is success.
   `:http-code': The http code returned by the request, if available.
   `:headers': An alist of the http headers.  Duplicated headers are coalesced into
               list.
   `:stderr': Anything curl returns on stderr.
   `:json': An alist representing any JSON returned by the server.
   "

  ;; Check the args
  (assert url)
  (if auth
      (assert (search ":" auth)))
  (assert (not (and body json)))
  (if timeout
      (assert (integerp timeout)))

  ;; Build up the command list
  (let ((cmd (list "curl" "-f" "--verbose" "--silent"))
        (json (when json
                (if (typep json 'string)
                    json
                  (json-encode json)))))
    (cl-flet ((append-option (arg value-fn)
                (when arg
                  (setf cmd (append cmd (funcall value-fn))))))
      (append-option op (| (list (upcase (format "-X%s" op)))))
      (append-option insecure (| `("--insecure")))
      (append-option auth (| `("--user" ,auth)))
      (append-option body (| `("--data-raw" ,body)))
      (append-option json (| `("-H" "Content-Type: application/json"
                               "--data-raw" ,json)))
      (append-option file (| `("--data-binary" ,file)))
      (append-option timeout (| `("--connect-timeout" ,(format "%d" timeout))))
      (append-option t (| list (if params
                                   (concat url "?" (url-encode-params params))
                                 url)))

      (message "Web-request running %s" cmd)

      ;; TODO(mls): better error handling.
      ;; it would be good to handle the error code + headers
      (let* ((resp (do-cmd cmd :stdout 'string :stderr 'string :throw throw))
             (output (assoc1 :stdout resp))
             (resp-json (ignore-errors
                          (json-read-from-string (assoc1 :stdout resp))))
             ;; Split out the '< content-type: application/json' headers
             ;; from curl, and turn them into an alist.
             (resp-block (parse-http-header-block (assoc1 :stderr resp))))

        (let ((http-code (if (equal (assoc1 :code resp) 0)
                             (string-to-int (second (assoc1 :resp-line resp-block)))
                           -1))
              (headers (assoc1 :headers resp-block)))

          (message "Web-request. code: %s http: %s\n headers: %s"
                   (assoc1 :code resp) http-code headers)

          `((:resp . ,output)
            (:code . ,(assoc1 :code resp))
            (:http-code . ,http-code)
            (:headers . ,headers)
            (:stderr . ,(when (not (equal (assoc1 :code resp) 0))
                          (assoc1 :stderr resp)))
            (:json . ,resp-json)))))))

(defun normalize-dir-path (path)
  (string-remove-suffix "/" (expand-file-name path)))

(defun dumpenv ()
  (mapcar (lambda (var)
            (if-let (key-value (string-find "^\\([^=]+\\)=\\(.+\\)$" var))
                (apply #'cons key-value)
              (cons var nil)))
          process-environment))

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

(defun open-custom-terminal (input-script name)
  (let ((buffer (generate-new-buffer (format "*term-%s-term*" name))))
    (with-current-buffer buffer
        (term-mode)
        (term-exec buffer "terminal" "/bin/bash" nil nil)
        (insertf "%s" input-script)
        (term-char-mode))
    (switch-to-buffer buffer)
   buffer))

;;
;; this is supposed to be for matching lists of alists.
;; I need to have a way to specify what data is returned.
;;
(defun assoc-query (query alist)
  (cl-labels ((match-query (query item)
                 (message "query: %s item: %s" query item)
                 (let ((cmd (car query))
                       (rest (cdr query)))
                   (message "cmd: %s rest: %s first rest %s" cmd rest (first rest))
                   (ecase cmd
                     (and (and (mapcar (| match-query % item) rest)))
                     (or  (or  (mapcar (| match-query % item) rest)))
                     (not (not (match-query rest item)))
                     (key (assoc (first rest) item))
                     (value (rassoc (first rest) item))))))

    (first (remove-if-not (| match-query query %) alist))))


;; TODO: Work on this.
;; It would be nice if it filled in the file names with
;; tab completion.
(defun tail-buffer (file)
  (interactive "sfile: ")
  (switch-to-buffer))

;; (defun find-virtualenv-file (root-dir)
;;   (car (first
;;         (sort (mapcar (fn (path)
;;                           (cons path (cond
;;                                       ((equal (normalize-dir-path (file-name-directory path))
;;                                               (normalize-dir-path root-dir)) 10)
;;                                       ((search "/target/" path) 5)
;;                                       (t 0))))
;;                       (directory-files-recursively root-dir "^activate$"))
;;               (fn (a b) (> (cdr a) (cdr b)))))))


;; (defun export-org-table ()
;;   (interactive)
;;   (unless (org-at-table-p)
;;     (error "No table at the point."))

;;   (save-excursion
;;     (org-table-begin)
;;     )
;;   )

(provide 'elisp-lib)
