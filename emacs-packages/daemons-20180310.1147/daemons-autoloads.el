;;; daemons-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "daemons" "daemons.el" (23233 10386 257031
;;;;;;  826000))
;;; Generated autoloads from daemons.el

(autoload 'daemons-status "daemons" "\
Show the status of the daemon NAME.

\(fn NAME)" t nil)

(autoload 'daemons-start "daemons" "\
Start the daemon NAME.  Show results in an output buffer.

\(fn NAME)" t nil)

(autoload 'daemons-stop "daemons" "\
Stop the daemon NAME.  Show results in an output buffer.

\(fn NAME)" t nil)

(autoload 'daemons-restart "daemons" "\
Restart the daemon NAME.  Show results in an ouptut buffer.

\(fn NAME)" t nil)

(autoload 'daemons-reload "daemons" "\
Reload the daemon NAME.  Show results in an output buffer.

\(fn NAME)" t nil)

(autoload 'daemons "daemons" "\
Open the list of system daemons (services) for user management.

This opens a ‘daemons-mode’ list buffer.  Move the cursor to a daemon line and
execute one of the commands in `describe-mode' to show status and manage the
state of the daemon.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("daemons-brew.el" "daemons-pkg.el" "daemons-systemd.el"
;;;;;;  "daemons-sysvinit.el") (23233 10386 261554 734000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; daemons-autoloads.el ends here
