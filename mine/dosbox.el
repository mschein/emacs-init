(require 'elisp-lib)

(defconst dosbox-global-config "~/Library/Preferences/DOSBox 0.74-2 Preferences")
(defconst dosbox-exec "/Applications/DosBox/DOSBox.app/Contents/MacOS/DOSBox" "Dosbox exec")
(defconst dosbox-games "/Applications/DosBox/games/" "Where to search for dos games")

(defvar dosbox-game-table '() "alist of game name -> location mappings")

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



(defun dosbox-find-exec-names (dir)
  (mapcar #'basename
          (directory-files-recursively dir "\.\\(exe\\|com\\)$")))

(defun dosbox-build-metadata (dir)
  (let* ((execs (dosbox-find-exec-names dir))
         (filtered-execs (remove-if (| member (downcase %) '("setup.exe" "install.exe"))
                                    execs)))
    `((:dir . ,dir)
      (:exec . ,(if (= 1 (length filtered-execs))
                    (first filtered-execs)
                  ""))
      (:execs . ,execs)
      (:config-file . "")
      (:config . nil))))

(defun dosbox--list-games ()
  (cl-loop for game-dir-name in (list-directory-entries dosbox-games :dirs-only t)
           collect (cons (downcase game-dir-name)
                         (dosbox-build-metadata (path-join dosbox-games game-dir-name)))))

;; This isn't the right way to do this:
(defun dosbox-init-games (game-table)
  (setf dosbox-game-table game-table))

(defun dosbox-run (&rest cmd)
  ;; The real commands need extra quoting, since some of
  ;; it gets stripped off by bash.
  (run "bash" "-c" (concat (cmd-to-shell-string (cons dosbox-exec cmd))
                           " &")))

(defun dosbox-run-to-str (&rest cmd)
  (apply #'run-to-str dosbox-exec cmd))

(defun dosbox-version ()
  (let ((version (first (string-find "version \\([0-9.]+\\)" (dosbox-run-to-str "-version")))))
    (assert version)
    version))

(defun dosbox-open-game (name)
  (interactive (list (completing-read "Game: " (assoc-keys dosbox-game-table) nil t)))

  (let* ((data (assoc1 name dosbox-game-table))
         (exec (path-join (assoc1 :dir data) (assoc1 :exec data)))
         (config-file (cdr (assoc :config-file data))))

    (message "Use Ctrl+F11 and Ctrl+F12 to set the cycles.")
    (with-tempdir (:root-dir "/tmp")
      ;; Deal with any custom config that's not in a file.
      (let ((file-name "config.ini"))
        (when-let (adjustments (cdr (assoc :config data)))
          (barf (dosbox-ini-write adjustments) file-name)
          (setf config-file file-name)))

      (let ((extra-args '()))
        (when (string-has-val config-file)
          (append-atom! extra-args "-conf" config-file))

        (dosbox-run exec "-conf" "config.ini")

        ;; Keep the config file around long enough to start it.
        ;; This is gross, and I should find a better mechanism.
        (sleep-for 5)))))

(defun dosbox-open-global-config ()
  (interactive)
  (find-file dosbox-global-config))

(provide 'dosbox)
