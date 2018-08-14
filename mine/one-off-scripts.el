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
      ],
      tests_require=[
          # Unit test dependencies.
          \'mock\',
      ],
      include_package_data=True,
      packages=find_packages(),
      package_data={'': []},
      zip_safe=True
)")

(defconst *python-init-file* "# empty module file
")

(defun python-create-project (name)
  (interactive "sname: ")

  (message "Current directory: %s" default-directory)
  (unless (file-exists-p name)
    (message "make dir %s" name)
    (make-directory name)

    (pushd name
       ;; write setup.py
       (message "create setup.py")
       (message "current dir %s" default-directory)

       (with-temp-file "setup.py"
         (insertf *python-setup.py-file* name))

       (message "create dirs")
       (make-directory name)
       (make-directory "test")

       (message "create init.py")
       (with-temp-file (path-join name "__init__.py")
         (insertf *python-init-file*))

       ;; Run command to setup more stuff
       (message "setup git module")
       (run "git" "init")
       (let ((venv-dir ".venv"))
         (message "create virtualenv")
         (run "virtualenv" "-p" "python3" "--no-site-packages" venv-dir)
         (make-symbolic-link ".venv/bin/activate" "activate")))))

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
    (let ((backup-file-path (path-join buffer-backup-storage-dir current-file-path)))
      (message "Copy file to backup path: %s" backup-file-path)
      (save-buffer)
      (ensure-makedirs backup-file-path)
      (copy-file current-file-path backup-file-path t)

      (message "Commit the changes to git.")

      ;; Commit the changes.
      (git-commit-changes buffer-backup-storage-dir
                          :message (if (string-has-val message)
                                       message
                                     (format "Backup the %s file" buf-name))))))

(defun buffer-backup-diff ()
  (interactive)

  (let* ((name (buffer-name))
         (current-file-path (buffer-file-name))
         (backup-file-path (buffer-backup-backup-path name current-file-path)))

    (switch-to-buffer (shell-open-dir buffer-backup-storage-dir))
    (rename-buffer (generate-new-buffer-name (format "*diff-%s-diff*" name)))
    (insertf "diff -u \'%s\' \'%s\'" backup-file-path current-file-path)))

(defalias 'bb 'buffer-backup-to-shared-repo)
(defalias 'bd 'buffer-backup-diff)

(defun edit-etc-hosts ()
  "Open the /etc/hosts files for editing."
  (interactive)
  (find-file "/sudo::/etc/hosts"))

(provide 'one-off-scripts)
