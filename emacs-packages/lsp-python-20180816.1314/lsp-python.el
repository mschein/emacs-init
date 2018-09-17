;;; lsp-python.el --- Python support for lsp-mode -*- lexical-binding: t -*-

;; Copyright (C) 2017 Vibhav Pant <vibhavp@gmail.com>

;; Author: Vibhav Pant <vibhavp@gmail.com>
;; Version: 1.0
;; Package-Version: 20180816.1314
;; Package-Requires: ((lsp-mode "3.0"))
;; Keywords: python
;; URL: https://github.com/emacs-lsp/lsp-python

;;; Code:
(require 'lsp-mode)
(require 'lsp-common)

(defcustom lsp-python-server-args
  '()
  "Extra arguments for the python-stdio language server"
  :group 'lsp-python
  :risky t
  :type '(repeat string))

(defun lsp-python--ls-command ()
  "Generate the language server startup command."
  `("pyls" ,@lsp-python-server-args))

(lsp-define-stdio-client lsp-python "python"
			 (lsp-make-traverser #'(lambda (dir)
						 (directory-files
						  dir
						  nil
              "setup.py\\|Pipfile\\|setup.cfg\\|tox.ini")))
                         nil
                         :command-fn 'lsp-python--ls-command)

(provide 'lsp-python)
;;; lsp-python.el ends here
