;; -*- lexical-binding: t -*-
(require 'elisp-lib)

;; Make these configurable

(defgroup dosbox-module-config nil
  "Config values for dosbox path locations."
  :group 'dosbox)

(defcustom dosbox-global-config nil
  "Path to dosbox preference file."
  :type 'string)

(defcustom dosbox-exec nil
  "Path to the dosbox exec."
  :type 'string
  :options '("/Applications/DosBox/DOSBox.app/Contents/MacOS/DOSBox"))

(defcustom dosbox-games nil
  "Where to search for dos games"
  :type 'string)

(defvar dosbox-game-table '() "alist of game name -> location mappings")

;; Feature ideas:
;;
;; - Add a link to the web page
;; - Add a link to the manual (open it as an option.)
;;   - local file or webpage.
;; - Add the ability (with a prefix arg) to just
;;   mount the dir, without running the exec.
;;

(defconst dosbox-ini-section-template
  "
#
# New section
#
[%s]
%s\n
")

(defun dosbox-ini-write (ini-lisp)
  ;;
  ;; The lisp code should be a alist of sections:
  ;; as in:
  ;; (dosbox .
  ;;           (language . nil)
  ;;           (captures . something)
  (cl-loop for (section . values) in ini-lisp
           concat (format dosbox-ini-section-template
                          section
                          (cl-loop for (key . value) in values
                                   concat (format "%s=%s\n" key (if value value ""))))))

(defun dosbox-find-configs (dir)
  (directory-files-recursively dir "^dosbox.conf$"))

(defun dosbox-find-app (dir)
  (cl-first
   (list-directory-entries dir
                           :dirs-only t
                           :full t
                           :match "\\.app$")))

(defun dosbox-find-exec-names (dir)
  (mapcar #'basename
          (directory-files-recursively dir "\\.\\(exe\\|com\\|bat\\)$")))

(defun dosbox-build-metadata (dir)
  (let* ((execs (dosbox-find-exec-names dir))
         (configs (dosbox-find-configs dir))
         (app (dosbox-find-app dir))
         (filtered-execs (remove-if (| member (downcase %) '("setup.exe" "install.exe"))
                                    execs)))
    (filter #'identity
            `((:dir . ,dir)
              ,(when app
                 (cons :app app))
              ,(when configs
                 (cons :config-file (cl-first configs)))
              (:exec . ,(if (= 1 (length filtered-execs))
                            (cl-first filtered-execs)
                          ""))
              (:execs . ,execs)))))

(cl-defun dosbox-list-games (&key skip-list)
  (cl-loop for game-dir-name in (list-directory-entries dosbox-games :dirs-only t)
           unless (member game-dir-name skip-list)
             collect (cons (downcase game-dir-name)
                           (dosbox-build-metadata (path-join dosbox-games game-dir-name)))))

(defun dosbox-sort-games (games)
  (sort* (copy-list games) #'string< :key #'car))

;; This isn't the right way to do this:
(defun dosbox-init-games (game-table)
  (setf dosbox-game-table game-table))

(defun dosbox-run (&rest cmd)
  (do-cmd-async (apply #'list "nohup" dosbox-exec cmd)
                :callback-fn (lambda (resp)
                               (message "dosbox command \"%s\" with code %s"
                                        cmd (assoc1 :code resp)))))

(defun dosbox-run-to-str (&rest cmd)
  (apply #'run-to-str dosbox-exec cmd))

(defun dosbox-version ()
  (let ((version (cl-first (string-find "version \\([0-9.-]+\\)" (dosbox-run-to-str "-version")))))
    (assert version)
    version))

(defun dosbox-open-game (name)
  (interactive (list (completing-read "Game: " (assoc-keys dosbox-game-table) nil t)))

  (let* ((data (assoc1 name dosbox-game-table))
         (app (assoc-get :app data)))
    (if app
        (run "open" app)
      (let* ((dir (assoc1 :dir data))
             (exec (assoc1 :exec data))
             (exec-args (assoc-get :exec-args data))
             (config-file (assoc-get :config-file data))
             (full-screen (assoc-get :full-screen data)))

        (message "Use Ctrl+F11 and Ctrl+F12 to set the cycles.")
        (message "Use alt (no fn) enter for full screen.")

        (with-tempdir (:root-dir "/tmp"
                       ;; This is a little gross, but seems to help
                       :delay-cleanup-sec 5)
          ;; Deal with any custom config that's not in a file.
          (let ((file-name "config.ini"))
            (when-let (adjustments (assoc-get :config data))
              (barf (dosbox-ini-write adjustments) file-name)
              (setf config-file file-name)))

          (let ((extra-args '()))
            (when (string-has-value-p config-file)
              (append-atom! extra-args "-conf" config-file))

            (when full-screen
              (append-atom! extra-args "-fullscreen"))

            (apply #'dosbox-run (append
                                 (if exec-args
                                     `("-c" ,(format "mount C %s" dir)
                                       "-c" "C:"
                                       "-c" ,(format "%s %s" exec exec-args))
                                   (list (path-join dir exec)))
                                 extra-args))))))))

(defun dosbox-get-game-data (game-name)
  (assoc1 game-name dosbox-game-table))

(defun dosbox-open-config ()
  (interactive)
  (find-file dosbox-global-config))

(defun dosbox-open-gamedir (name)
  (interactive (list (completing-read "Game: " (assoc-keys dosbox-game-table) nil t)))

  (if-let (dir (assoc-get :dir (dosbox-get-game-data name)))
      (shell-open-dir dir)
    (message "No game directory registered for %s" name)))

(defun dosbox-open-manual (name)
  (interactive (list (completing-read "Game: " (assoc-keys dosbox-game-table) nil t)))
  (if-let (manual (assoc-get :manual (dosbox-get-game-data name)))
      (do-cmd-async (list "open" "-a" "Preview" manual))
    (message "No manual found for %s" name)))

(provide 'dosbox)
