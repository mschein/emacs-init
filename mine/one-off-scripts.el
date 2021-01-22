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

;; (defun make-abbrev-list (list)
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
;;            list)
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
  (find-file-noselect "~/doc/notes.org")
  (switch-to-buffer-other-window "todo.org"))


(defun find-dup (list)
  ;; loop though list
  ;; get element at random length
  ;; how do you know which ones to
)

(defun slope (v1 v2)
  (/ (- (cadr v1) (cadr v2))
     (- (car v1) (car v2))))

(defun find-emacs-unsaved-files ()
  (interactive)
  (insertf "find . | grep \\#"))

;; Some useful bookmarks
(make-bookmark clojure-doc "http://clojuredocs.org/")
(make-bookmark clojure-ref "http://clojuredocs.org/quickref/Clojure%20Core")


(defun sql-insert-select (table-name)
  (insertf
   "

def %s_select(%s_id, conn=engine):
  return conn.execute(select([%s]).where(%s.c.id==%s_id)).fetchone()
" table-name table-name table-name table-name table-name)
  (insertf
   "

def %s_select_and(conn=engine, **columns):
  args = [column==columns[column.name] for column in %s.c if column.name in columns.keys()]
  return conn.execute(select([%s]).where(and_(*args))).fetchall()
" table-name table-name table-name table-name table-name)
  (insertf
   "

def %s_select_or(conn=engine, **columns):
  args = [column==columns[column.name] for column in %s.c if column.name in columns.keys()]
  return conn.execute(select([%s]).where(or_(*args))).fetchall()
" table-name table-name table-name table-name table-name))

(defun sql-insert-insert (table-name)
  (insertf
   "

def %s_insert_group(args, conn=engine):
  return conn.execute(%s.insert(), args)
" table-name table-name)

  (insertf
   "

def %s_insert(conn=engine, **kwargs):
  result = conn.execute(%s.insert(), **kwargs)
  return result.inserted_primary_key[0]
" table-name table-name))

(defun sql-insert-update (table-name)
  (insertf
   "

def %s_update(%s_id, conn=engine, **kwargs):
  # HACK until we move to sqlalchemy 0.8
  unknown_columns = set(kwargs.keys()) - set(%s.c.keys())
  if unknown_columns:
    raise ValueError('some column names were invalid in the update set: %%s' %% unknown_columns)
  result = conn.execute(%s.update().
                        where(%s.c.id==%s_id).
                        values(**kwargs))
  if result.rowcount < 1:
    raise ValueError('no rows updated')
  return result
" table-name table-name table-name table-name table-name table-name))

;;(defun sql-insert-delete (table-name))

(defun sql-insert-crud (table-name)
  (interactive "stable-name: ")
  (dolist (fn '(sql-insert-select
                sql-insert-insert
                sql-insert-update))
    (funcall fn table-name)))

(defun sql-insert-crud-for-list (table-names)
  (dolist (table table-names)
    (sql-insert-crud table)))

(defun sql-get-table-names-region (begin end)
  (interactive "r")
  (->> (region-to-list begin end)
       (remove-if-not (| string-match-p " = Table(" %))
       (mapcar (| first (string-find-all "\\([^ ]+\\) = Table" %)))))

(defun sql-insert-crud-for-region (begin end)
  (interactive "r")
  (sql-insert-crud-for-list (sql-get-table-names-region begin end)))

(defun conv-cool-constructor (begin end)
  (->> (region-to-list begin end)
       )
  )

(defun find-all-connected-hosts ()
  (interactive)
  (let ((buffer (get-buffer-create "*local-network*")))
    (clear-buffer buffer)
    (shell-command "ping -c 3 192.168.1.255")
    (shell-command "arp -a" buffer)
    (switch-to-buffer buffer)))
;;
;; keeping this for reference
;;
;; (defun csv-normalize-ids-internal (&optional start-line start-number)
;;   (let ((start-line (or start-line 1))
;;         (start-number (or start-number 1)))

;;     (save-excursion
;;       ;; go back to the top and n lines down.
;;       (goto-char (point-min))
;;       (next-line start-line)
;;       (message "Im here")

;;       (let ((num start-number)
;;             (total-lines (count-lines (point-min) (1+ (buffer-size)))))
;;         (while (< (line-number-at-pos) total-lines)
;;           (message "num is %d total-lines %d\n" num total-lines)
;;           (beginning-of-line)
;;           (replace-regexp "^[0-9]+" (number-to-string num))
;;           (incf num)
;;           (next-line))))))


(defun csv-normalize-ids-internal (&optional start-line start-number)
  (let ((start-line (or start-line 1))
        (number (or start-number 1)))

    (save-excursion
      ;; go back to the top and n lines down.
      (goto-char (point-min))
      (next-line start-line)
      (while (re-search-forward "^[0-9]+" nil t)
        (replace-match (number-to-string number) nil nil)
        (incf number)))))

(defun csv-normalize-ids ()
  (interactive)
  (csv-normalize-ids-internal))

(defun jsx-mode ()
  (interactive)
  (web-mode)
  ;; This is ugly to do with flymake, I should consider switching
  ;; to flycheck.
  (make-local-variable 'flymake-allowed-file-name-masks)
  (setf flymake-allowed-file-name-masks (list '(".*\\'" flymake-eslint-init)))
  (flymake-mode t))

(defconst *python-setup.py-file*
  "from setuptools import setup, find_packages

# Use https://pyinstaller.readthedocs.io/en/stable/
# for install.

package = '%s'
version = '0.1'

setup(name=package,
      version=version,
      description=\"<some description>\",
      entry_points={
          'console_scripts': []
      },
      install_requires=[
          # Macros can be had with mcpy and macropy
          # If this is a reusable component, put
          # requirements here with ranges.
      ],
      tests_require=[
          # Unit test dependencies.
          \'mock\',
          \'pytest\'
      ],
      include_package_data=True,
      packages=find_packages(),
      package_data={'': []},
      zip_safe=True
)")

(defconst *requirements.txt*
  "# Put requirements here if this is an application.
# you can also create this with pip freeze

")

(defconst *python-init-file* "# empty module file
")

(defun python-init-basic-dir (dir)
  (pushd dir
    (message "setup git module")
    (run "git" "init")
    (let ((venv-dir ".venv"))
      (message "create virtualenv")
      (run "python3" "-m" "venv" venv-dir)
      (make-symbolic-link ".venv/bin/activate" "activate"))))

(defun python-create-project (dir)
  (interactive "Fdir: ")

  (let ((name (basename dir)))
    (message "Create project directory %s for %s" dir name)
    (make-directory dir)

    (pushd dir
      ;; write setup.py
      (message "create setup.py")
      (message "current dir %s" default-directory)

      (cl-loop for (file . contents) in `(("setup.py" . ,*python-setup.py-file*)
                                          ("requirements.txt" . ,*requirements.txt*)
                                          ("tox.ini" . "# Put any tox stuff here.")
                                          ("README.md" . ,(format "My awesome project %s" name))
                                          ("MANIFEST.in" . "# recursive-include app/templates *")
                                          ("CHANGES.txt" . ""))
               do (barf contents file))

      (message "create dirs")
      (make-directory name)
      (make-directory "test")

      (message "create init.py")
      (barf *python-init-file* (path-join name "__init__.py"))

      ;; Run command to setup more stuff
      (python-init-basic-dir default-directory))))

(defun python-create-bare-project (dir)
  (interactive "Fdir: ")

  (let ((project-name (basename dir)))
    (message "Create project directory %s for %s" dir project-name)
    (make-directory dir)

    (pushd dir
      (python-init-basic-dir dir))))

(defun python-create-django-project (dir)
  (interactive "Fdir: ")
  (python-create-bare-project dir)

  (with-venv dir
    ;; Add a docker file?
    (message "Install django")
    (run "pip3" "install" "Django")
    (run "django-admin" "startproject" (basename dir))))

;; May want to add more metadata to these.
(defconst *cl-readme* "# ${name}
### _Your Name <your.name@example.com>_

<project info>

## License

Specify license

")

(defconst *cl-asd* ";;;; ${name}.asd

(asdf:defsystem #:${name}
  :description \"Describe ${name} here\"
  :author \"Your Name <your.name@example.com>\"
  :license  \"Specify license here\"
  :version \"0.0.1\"
  :serial t
  :depends-on (${deps})
  :components ((:file \"package\")
               (:file \"${name}\")))
")

(defconst *cl-main* ";;;;
;;;; ${name} main file.
;;;;

;;
;; Style Guide: https://lisp-lang.org/style-guide
;;

(in-package #:${name})
")

(defconst *cl-cli-main* ";;;;
;;;; ${name} main file.
;;;;

;;
;; Style Guide: https://lisp-lang.org/style-guide
;;
(in-package #:${name})

;; (defun environment->alist ()
;;   (mapcar (lambda (elm)
;;             (destructuring-bind (key &optional value) (cl-ppcre:split \"=\" elm)
;;               (cons key value)))
;;           (sb-ext:posix-environ)))

;; Pretty print a hash table
;; (defmethod print-object ((object hash-table) stream)
;;   (format stream \"#HASH{~{~{(~a : ~a)~}~^ ~}}\"
;;           (loop for key being the hash-keys of object
;;                 using (hash-value value)
;;                 collect (list key value))))

(defvar *cli-args*
  '(:start))

(defun main ()
  (cli-args:main-handler *cli-args*))

")

(defconst *cl-build-cli-sh* "#!/bin/bash -ev

# Run this to create a lisp executable.

rm -f ${name}

sbcl --no-sysinit \\
     --no-userinit \\
     --load build-cli.lisp

# install ./${name} ~/bin/
# cp ./${name} /Volumes/Documents/software/binaries/

")

(defconst *cl-build-cli-lisp* "(declaim (optimize (speed 3) (space 3) (debug 0)))

;; Load quicklisp into the image.
(let ((quicklisp-init (merge-pathnames \"quicklisp/setup.lisp\"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :${name})

(in-package :${name})

(sb-ext:save-lisp-and-die \"${name}\"
                          :toplevel #'main
                          :executable t
                          :save-runtime-options t
                          :purify t
                          ;; Note! this does make a difference
                          ;; The trade off is startup time vs
                          ;; executable size.
                          ;; nil: 67MB vs .06s to start
                          ;;   1: 17MB vs .2s
                          ;;   5: 15MB vs .2s to start (noticable lag)
                          ;;   9: 14MB vs .2s to start
                          :compression nil)
")

(defconst *cl-cli-readme* "# ${name}
### _Your Name <your.name@example.com>_

<project info>

## To build the CLI, do:
./build-cli.sh

## License

Specify license

")

(defconst *cl-package* ";;;; package.lisp

(defpackage #:${name}
  (:use #:cl #:mu))
")

(defconst *cl-gitignore* "*.fasl
${name}")

(defconst *common-lisp-standard-projects* '(("lib" . ())
                                            ("cli" . ("cli-args"))
                                            ("web" . ("drakma"
                                                      "hunchentoot"
                                                      "easy-routes"
                                                      "sqlite"))))

(defun padding (length)
  ;; There's probably a smarter way to do this.
  (string-join (cl-loop for x below length collect " ") ""))

(defun common-lisp-build-deps (type)
  (let ((default-deps '("alexandria"
                        "cl-interpol"
                        "cl-ppcre"
                        "closer-mop"
                        "let-plus"
                        "prove"
                        "serapeum"
                        "rutils"
                        "rutilsx"
                        "split-sequence"
                        "uiop"
                        "mu"
                        ))
        (padding (padding (length "  :depends-on ("))))

    (string-join (mapcar (| format "#:%s" %) (concatenate 'list default-deps
                                                          (assoc1 type *common-lisp-standard-projects*)))
                 (concat "\n" padding))))

(defun common-lisp-create-project (type name)
  (interactive (list (completing-read "type: " (assoc-keys *common-lisp-standard-projects*) nil t)
                     (read-string "name: ")))

  (assert (not (search " " name)) nil "Dont put spaces in the name")

  (message "Current directory: %s" default-directory)

  (when (file-exists-p name)
    (error (format "Directory %s exists, abort common lisp create." name)))

  (let* ((target (path-join default-directory name))
         (template-vars `(("name" . ,name)
                          ("deps" . ,(common-lisp-build-deps type))))
         (is-cli (equal "cli" type)))
    (message "Create project %s" name)
    (ensure-makedirs target)
    (pushd target
      ;; Do I want to keep the is-cli check?
      (cl-loop for (file template) in `(("README.md" ,(if is-cli
                                                          *cl-cli-readme*
                                                        *cl-readme*))
                                        (".gitignore" ,*cl-gitignore*)
                                        (,(format "%s.asd" name) ,*cl-asd*)
                                        (,(format "%s.lisp" name) ,(if is-cli
                                                                       *cl-cli-main*
                                                                     *cl-main*))
                                        ("package.lisp" ,*cl-package*))
               do (barf (string-template-fill template template-vars)
                        file))
      (if is-cli
          (cl-loop for (file template) in `(("build-cli.sh" ,*cl-build-cli-sh*)
                                            ("build-cli.lisp" ,*cl-build-cli-lisp*))

                   do (progn
                        (barf (string-template-fill template template-vars)
                              file)

                        ;; This should probably be explicit, you know
                        ;; have extra arguments witih the template strings.
                        (when (equal "sh" (file-name-extension file))
                          (chmod file #o755)))))
      (message "Setup git repo in %s" target)
      (git-init-repo "."))))

(defun unique-file-name (name index)
  "Make a unique name but save the extension"
  (format "%s-%d%s"
          (file-name-sans-extension name)
          index
          (if-let (ext (file-name-extension name))
              (concat "." ext)
            "")))

(defun make-unique-file-name (dir file-name)
  (let ((final-path (path-join dir file-name))
        (index 0))
    (when (file-exists-p final-path)
      ;; Make it unique
      (incf index)
      (setf final-path (unique-file-name final-path index))
      )
    final-path))

(defconst buffer-backup-storage-dir (expand-file-name "~/backup"))
(defconst buffer-backup-transient-dir (expand-file-name "~/transient-files"))

(defun buffer-backup-file-path (current-file-path)
  (path-join buffer-backup-storage-dir current-file-path))

(defun buffer-backup-to-shared-repo (message)
  (interactive "smessage: ")
  ;;
  ;; Make it a git repo (doing a re-init is safe according to git's) docs.
  ;;
  (message "Setup directories and init repo.")
  (ensure-makedirs buffer-backup-storage-dir)
  (ensure-makedirs buffer-backup-transient-dir)
  (git-init-repo buffer-backup-storage-dir)

  (let* ((buf-name (buffer-name))
         (current-file-path (buffer-file-name))
         (original-default-dir default-directory))

    ;; Save transient files to a special directory.
    (unless current-file-path
      (setf current-file-path (make-unique-file-name buffer-backup-transient-dir buf-name))
      (message "Save transient file to %s" current-file-path)
      (write-file current-file-path)
      (set-default-directory original-default-dir))

    ;;
    ;; Commit the file to disk, but don't try to save it directly to the
    ;; backup directory, since that will mess stuff up which disk file
    ;; is associated with a buffer.  However, if the buffer has no
    ;; on disk file, than it can save directly to the backup directory.
    ;;
    (let ((backup-file-path (buffer-backup-file-path current-file-path)))
      (message "Copy file to backup path: %s" backup-file-path)
      (save-buffer)
      (ensure-makedirs (file-name-directory backup-file-path))
      (copy-file current-file-path backup-file-path t)

      (message "Commit the changes to git.")

      ;; Commit the changes.
      (git-commit-changes buffer-backup-storage-dir
                          :message (if (string-has-value-p message)
                                       message
                                     (format "Backup the %s file" buf-name))))))

(defun buffer-backup-diff ()
  (interactive)

  (let* ((name (buffer-name))
         (current-file-path (buffer-file-name))
         (backup-file-path (buffer-backup-file-path current-file-path)))
    (unless (file-exists-p backup-file-path)
      (error "File %s is not in backup repository at %s.  Giving up"
             current-file-path backup-file-path))

    (switch-to-buffer (shell-open-dir buffer-backup-storage-dir))
    (rename-buffer (generate-new-buffer-name (format "*diff-%s-diff*" name)))
    (insertf "diff -u \'%s\' \'%s\'" backup-file-path current-file-path)))

(defalias 'bb 'buffer-backup-to-shared-repo)
(defalias 'bd 'buffer-backup-diff)

(defun edit-etc-hosts ()
  "Open the /etc/hosts files for editing."
  (interactive)
  (find-file "/sudo::/etc/hosts"))

(defun flymake-diagnostic-at-point ()
  (interactive)
  (message "%s" (flymake-diagnostic-at-point-get-diagnostic-text)))


(defun docker-shell-to-local-container (container)
  ;;
  ;; docker ps
  ;; docker 6c113e64bb4e)
  ;; docker exec -it 6c113e64bb4e /bin/bash
  ;;
  )

(defun validate-yaml-file (path)
  "Return nil if the file is fine.  Return the error message if the file is
   bad."
  (let ((resp (do-cmd (list "python" "-c" "import yaml, sys; yaml.safe_load(sys.stdin)") :input path)))
    (when (not (eql 0 (assoc1 :code resp)))
      (assoc1 :stderr resp))))

(defun combinations (first &rest rest)
  "Args is a list of lists essentially"
  (if (not rest)
      first
    (mapcan (lambda (elm)
              (mapcar (lambda (x) (cons elm (to-list x)))
                      (apply #'combinations rest)))
            first)))

(defun osx-install-command-line-tools ()
  (interactive)
  (run "xcode-select" "--install"))

;;(defun jenv-list-versions ()
;;  ())

;;
;; setting up selenium with python:
;;
;; brew install geckodriver
;; brew cask install chromedriver
;;
;; Create the python project.
;; pip install selenium
;;

(provide 'one-off-scripts)
