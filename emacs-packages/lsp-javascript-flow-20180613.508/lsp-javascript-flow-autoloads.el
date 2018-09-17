;;; lsp-javascript-flow-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "lsp-javascript-flow" "lsp-javascript-flow.el"
;;;;;;  (23453 48885 0 0))
;;; Generated autoloads from lsp-javascript-flow.el

(defvar lsp-javascript-flow-server "flow-language-server" "\
The flow-language-server executable to use.
Leave as just the executable name to use the default behavior of
finding the executable with `exec-path'.")

(custom-autoload 'lsp-javascript-flow-server "lsp-javascript-flow" t)

(defvar lsp-javascript-flow-server-args 'nil "\
Extra arguments for the javascript-flow-stdio language server")

(custom-autoload 'lsp-javascript-flow-server-args "lsp-javascript-flow" t)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; lsp-javascript-flow-autoloads.el ends here
