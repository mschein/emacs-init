;;
;; Copyright (C) by Michael Scheinholtz.  All Rights Reserved.
;;
;; TODO:
;; 1. reorg doc here.
;; 2. Need to figure out tab and hippie expand to
;;    get everything to play nicelyl
;;
;; find Jedi
;;
;; Orgmode notes
;; to get Emacs documentation:
;; M-x info (lots of stuff here)
;; `C-c C-x C-i' clock start `C-c C-x C-o' clock stop `C-c C-y' recompute time
;;
;; ... M-x server-start to make one emacs session the server
;; emacsclient -t for terminal access
;; emacsclient -c for X11 access.
;;
;; ielm for command loop
;;;;
;; eshell vs shell.... interesting to try.
;; Newline in a regex:
;; c-q c-j
;; occur !!! show all lines (across buffers) matching a regex
;;
;; using lisp in regexes substitutions:
;; \,(lispcode): can call any function, they can use (matched-regex 1) or whatever the function
;; is.
;; also, you can use \1, \2, for different matches.
;;
;;M-x dirs will tell the shell buffer to figure out what the current working directory is.
;;
;; find-library can be useful for finding more documentation
;;
;;Searching through multiple buffers:
;; (multi-occur-in-matching-buffers)
;;
;;C-/
;;
;; SLIME Coolness:
;;  c-c c-d c-d  -> Print the doc string of any function.
;;
;;
;;Macro recording [permalink]
;;C-x ( : start recording keyboard macro
;;C-x ) : stop recording keyboard macro
;;C-x e : replay current keyboard macro
;;
;; starting common lisp slime
;; (load-file slime-lisp.el)
;;
;; run lein -swank and then M-x slime-connect to run
;; slime with clojure.
;;
;; Stuff to know
;; ielm: emacs LISP repl
;; shell: shell in emacs
;; man: man in emacs.
;; ediff: diff 2 files
;; elisp-index-search: find help for commands
;; package updater: M-x package-list-packages
;;
;; to turn off annoying case issues, do:
;; M-x set-variable: case-fold-search to nil
;;
;; remember google supports g4
;;
;; M-x package-list-packages
;;
;; eshell might be fun instead of shell
;;
;; find-* has useful file searching commands
;; dired is good for moving bunches of files.
;; Q in dired query replaces thigns in multiple files.
;;
;; Need to do some of the following:
;; 2. apply some thought to the design of my utilities
;;    a. what do I do all of the time that could be made faster.
;;       1. directory traversal
;;       2. running scripts and commands and processing the output
;;       3. SRE/Sysadmin type tasks (what would make these faster)
;;       4. need to remember urls/file paths/bigtable/megastore paths.
;;       2. mostly it's pretty good so far.
;;          -> coming up with a good shell driver will be cool
;;          -> perl + lisp + super programmable
;;
;;    c. Other interesting features.
;;       1. networking (simple smtp, imap, ftp, www)?
;;       2. file moving between systems?
;;       3. splitting off commands and getting their output
;;       4. chains of commands that allow processing in between and
;;          automatically deal with error handling.
;;       5. automatically launch a webbrowser at a specific page
;;       6. webscraping?
;;       7. better csv display?
;;       8. log analysis?
;;
;; icicles? ... super search for everything http://www.emacswiki.org/cgi-bin/wiki/Icicles
;;
;; Re-tabify
;; why aren't backups working?
;;
;; reload current file.
;; learn more of ido.
;;
;; Useful macros:
;;   interactive?
;;   to-new-buffer?
;;   to-console?
;;
;; 3. learn about hooks, and get a better grasp of how to manipulate emacs.
;;
;; Need a notes file,
;;
;; If I need a stack, queue, avl tree, look in to elib (obsolete)
;;
;; More useful commands:
;; 1. yank string: copy the double quoted string into the kill ring
;; 2. go to path: takes a java import or a google //path
;;
;; Design
;;
;; one off scripts
;; Library
;;
;; indent-region and what not is awesome.
;;
;; Term Mode doc
;; c-c c-j : line mode (more emacs like
;; c-c c-k: char mode
;;
;; c-c c-c: send a literal c-c
;; c-c CHAR: like c-x char normally in char mode.
;;
;; manual font size adjustments
;; C-x C-+,   C-x C--
;;
;; Need to add a command to run doc find on a function
;;
;; Toggle readonly c-x c-q
;;
;; Debugging:
;; M-X check-parens to find unbalanced stuff.
;;
;; Fix font... I am not using the anti-aliased fonts.
;;
;; Global includes
;;
;; c-x c-k l:Turn the last 100 commands into a macro
;;
;; More emacs key strokes.
;; http://www.cs.rutgers.edu/LCSR-Computing/some-docs/emacs-chart.html
;;
;; Mastering eshell:
;;  http://www.masteringemacs.org/articles/2010/12/13/complete-guide-mastering-eshell/
;;
(require 'cl)
(require 'find-lisp)
;; (require 's)
;; (require 'dash)
;; (require 'dash-functional)
;(require 'cssh)

;; My stuff
(dolist (path '("~/emacs-init/mine" "~/emacs-init/contrib" "~/emacs-init/contrib/groovymode" "~/emacs-init/elpa/http-post-simple-1.0" "~/emacs-init/autopair" "~/emacs-init/company"))
  (if (file-directory-p path)
      (add-to-list 'load-path path)))

;; Always load these.
(require 'elisp-lib)
(require 'one-off-scripts)

;; Deal with anything in the company directory.
(let ((company-dir "~/emacs-init/company"))
  (if (file-directory-p company-dir)
      (dolist (file-path (directory-files company-dir 'full ".el$"))
	(load file-path))))


;; Add the new emacs package loader:



;; For Clojure work use nReple and ritz


;; To run a slime, uncomment and slime-lisp and start
;; a new emacs.
(require 'slime-lisp)

;; Add more info paths
(add-to-list 'Info-default-directory-list "/usr/local/share/info")

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/emacs-init/elpa/package.el"))
  (package-initialize))

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)


;; Set up the keyboard so the delete key on both the regular keyboard
;; and the keypad delete the character under the cursor and to the right
;; under X, instead of the default, backspace behavior.
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)

;; turn on font-lock mode
(global-font-lock-mode t)
;; enable visual feedback on selections
(setq-default transient-mark-mode t)

;; always end a file with a newline
(setq require-final-newline t)

;; stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

(setq line-number-mode 1)
(setq column-number-mode 1)
(show-paren-mode 1)

;;
;; Get cperl-mode setup since it is better.
;;
(defalias 'perl-mode 'cperl-mode)

(setq cperl-close-paren-offset -4)
(setq cperl-continued-statement-offset 4)
(setq cperl-indent-level 4)
(setq cperl-intent-parens-as-block t)
(setq cperl-tab-always-indent t)

;; Turn tabs off
(setq-default indent-tabs-mode nil)

;;
;; Mike's customizations
;;
(setq c-basic-offset 4)

;; Make goto-line a command.
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-x\M-f" 'find-file-other-window)

;; Add ruby files.
(add-to-list 'load-path "~/emacs-init/ruby-mode")
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(autoload 'ruby-mode "ruby-mode" "Major mode for ruby code")

;; Auto indentation
(add-hook 'ruby-mode-hook (lambda () (local-set-key "\r" 'newline-and-indent)))

;; Ruby inside emacs
(require 'inf-ruby)

;; Want one function to handle this, since this function changes when we upgrade
(defun turn-on-subword-mode ()
  (subword-mode))

;; Javascript for json editing.
(add-to-list 'load-path "~/emacs-init/javascript")
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
(autoload 'javascript-mode "javascript" nil t)
(add-hook 'javascript-mode 'turn-on-subword-mode)

;; Allow these functions.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell Mode options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Emacs now supports colors :-)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; But it doesn't support less, so turn the pager off.
(setenv "GIT_PAGER" "")

;; Set Chrome as the default browser
(setq browse-url-generic-program "/opt/google/chrome/google-chrome")

;; Smarter buffer switching
;;
;; also see:
;;
;; If you want to prevent certain buffers from showing up in the completion list, set ‘iswitchb-buffer-ignore’. Example:
;; (setq iswitchb-buffer-ignore '("^ " "*Buffer"))
;; This one is useful if you want to lose the *…* special buffers from the list. It’s helpful if you’re using the JDEE for editing Java apps, as you end up with buffers named org.whatever.package.Class which you might want to eliminate:
;; (setq iswitchb-buffer-ignore '("^\\*"))
;;
(iswitchb-mode 1)

;; automatically unzip/gunzip/uncompress files
(auto-compression-mode 1)

;;
;; Turn on red highlighting for characters outside of the 80/100 char limit
(defun font-lock-width-keyword (width)
  "Return a font-lock style keyword for a string beyond width WIDTH
that uses 'font-lock-warning-face'."
  `((,(format "^%s\\(.+\\)" (make-string width ?.))
     (1 font-lock-warning-face t))))

(font-lock-add-keywords 'c++-mode (font-lock-width-keyword 80))
(font-lock-add-keywords 'java-mode (font-lock-width-keyword 100))
(add-hook 'java-mode 'turn-on-subword-mode)

;; Make cut and paste work
(setq x-select-enable-clipboard t)

;; Find files faster.  http://curiousprogrammer.wordpress.com/2009/02/18/emacs-directory-aliases/
;;
;; http://www.emacswiki.org/cgi-bin/wiki/InteractivelyDoThings#toc5
;; Use c-e to edit the path
;; c-b to switch buffers instead.
;; c-d to switch to dired.
;; c-f to go back to find-file.
;; c-j create a file.
;; // -> root
;; ~/ -> home
;;       -> Can I make more of these?
;;
;;
(ido-mode t)
(setq ido-enable-flex-matching t)

(setq ido-create-new-buffer 'always)

;;
;; Add YASnippet support
;;
(add-to-list 'load-path "~/emacs-init/plugins/yasnippet-0.6.1c")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/emacs-init/plugins/yasnippet-0.6.1c/snippets")
(setq require-final-newline nil)

;; Paredit setup
;; TODO Need to learn to use this.
;(autoload 'paredit-mode "paredit"
;  "Minor mode for pseudo-structurally editing Lisp code." t)
;(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
;(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
;(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))

;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
;; (defun override-slime-repl-bindings-with-paredit ()
;;   (define-key slime-repl-mode-map
;;     (read-kbd-macro paredit-backward-delete-key) nil))
;; (add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)


;; Shut off useless menu items. etc.
(setq inhibit-splash-screen t)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; pick a font.
;; (set-face-attribute 'default nil :height 80)


;; Magit options
(add-hook 'magit-mode-hook 'magit-load-config-extensions)


;;
;; Make window navigation easier
;;
;; TODO: Fixme.
(global-set-key [M-left]  'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up]    'windmove-up)
(global-set-key [M-down]  'windmove-down)

;; Thanks Steve.
(defalias 'qr  'query-replace "A short cut for query-replace")
(defalias 'qrr 'query-replace-regexp "A short cut for query-replace-regexp")
(defalias 'rr 'replace-regexp "A short cut for replace-regexp")
(defalias 'eb 'eval-buffer)
(defalias 'sl 'sort-lines)
(defalias 'ir 'indent-region)
(defalias 'i 'insert-interactive)
(defalias 'ia 'insert-author)
(defalias 'scc 'slime-connect-clj)
(defalias 'ms 'magit-status)
(defalias 'ml 'magit-log)

(defalias 'sb 'multi-occur-all)
(defalias 'sbx 'multi-occur-in-matching-buffers)

(defalias 'ms 'magit-status)

(defalias 'cr 'comment-region)
(defalias 'ucr 'uncomment-region)
(defalias 'shd 'shell-dir)
(defalias 'rlf 'reload-file)
(defalias 'ls 'linkedin-search)
(defalias 'ul 'underline)
(defalias 'ytp 'yank-to-file-location-python)


(global-set-key "\M-sb" 'multi-isearch-buffers)
(global-set-key "\M-sB" 'multi-isearch-buffers-regexp)

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key "\C-c\C-c" 'comment-region)
(global-set-key "\C-cu" 'uncomment-region)

(global-set-key "\M-p" 'backward-paragraph)
(global-set-key "\M-n" 'forward-paragraph)

(global-set-key "\C-cd" 'duplicate-line)
;;(global-set-key "\C-cg" 'jump-to-abbrev-li)

;; Save desktops
(desktop-save-mode 1)

;; put all backup files in one place
(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.emacs-backups"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 8
      kept-old-versions 8
      version-control t)      ; use versioned backups

;;
;; Make buffer names smarter
;;
(require 'uniquify)
(setq uniquify-separator "/")
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniqify-ignore-buffers-re "^\\*")

(global-set-key "\C-c\C-r" 'reload-file)
(global-set-key "\C-cp" 'insert-client-path)
(global-set-key "\C-g" 'goto-line)
(global-set-key "\C-cj" 'import-jump)
(global-set-key "\C-xt" 'open-todo)
(global-set-key "\C-c\M-f" 'find-file-at-point)
(global-set-key (kbd "C-;") 'undo)

;; Flyspell code
(defun turn-on-flyspell ()
   "Force flyspell-mode on using a positive arg.  For use in hooks."
   (interactive)
   (flyspell-mode 1)
   (define-key flyspell-mode-map (kbd "C-;") 'undo))
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(add-hook 'text-mode-hook 'turn-on-flyspell)

;(add-hook 'c-mode-common-hook 'flyspell-prog-mode)


;; ;; Found on interwebs:
;; ;; http://emacsblog.org/2007/03/12/tab-completion-everywhere/
;; (defun indent-or-expand (arg)
;;   "Either indent according to mode, or expand the word preceding
;; point."
;;   (interactive "*P")
;;   (if (and
;;        (or (bobp) (= ?w (char-syntax (char-before))))
;;        (or (eobp) (not (= ?w (char-syntax (char-after))))))
;;       (dabbrev-expand arg)
;;     (indent-according-to-mode)))

;; (defun my-tab-fix ()
;;   (local-set-key [tab] 'indent-or-expand))

;; (add-hook 'c-mode-hook          'my-tab-fix)
;; (add-hook 'ruby-mode-hook       'my-tab-fix)
;; (add-hook 'sh-mode-hook         'my-tab-fix)
;; (add-hook 'emacs-lisp-mode-hook 'my-tab-fix)
;; ;;(add-hook 'slime-mode-hook 'my-tab-fix)
;; (add-hook 'clojure-mode-hook 'my-tab-fix)


;; from http://joost.zeekat.nl/2010/06/03/slime-hints-3-interactive-completions-and-smart-tabs/

;;
;; This might be the way to go.
;;
;; (setq hippie-expand-try-functions-list
;;       (append hippie-expand-try-functions-list '(slime-complete-symbol)))
;; (setq smart-tab-completion-functions-alist
;;       '((emacs-lisp-mode . lisp-complete-symbol)
;;         (text-mode . dabbrev-completion)
;;         (slime-repl-mode . slime-complete-symbol)))

;; Turn on auto completion.
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/emacs-init/contrib/ac-dict")
(ac-config-default)
(ac-flyspell-workaround)


;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; default to unified diffs
(setq diff-switches "-u")

;; always end a file with a newline
;(setq require-final-newline 'query)

;;; uncomment for CJK utf-8 support for non-Asian users
;; (require 'un-define)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode customizations.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; !!! NOTE !!! if you move emacs versions, be sure to recompile the files
;; in the org-mode directory.  Also, if you hack org mode...
;;
;; cd ~/emacs-init/contrib/org-mode/lisp
;; make
;;
;; make install-info to update the org-mode docs.
;;
;; export note: use pandoc to convert from latex to mediawiki.
;;  .cabal/bin/pandoc -f latex -t mediawiki <your file>
;; also, there is another ruby converter for org-mode files to make wikis
;;

;; A directory for non-default orgmode files.
(add-to-list 'load-path "~/emacs-init/contrib/org-mode/lisp")
(add-to-list 'load-path "~/emacs-init/contrib/org-mode/contrib/lisp")
(add-to-list 'load-path "~/emacs-init/contrib/org-mode/EXPERIMENTAL")

;; Install the custom org mode.
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; recommended key bindings from the manual.
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Add support for wiki exporting. !!! NOTE !!!
;; This touches experimental stuff, so it could easily break on upgrade.
(require 'org-mw)

;; linkedin wiki.
(require 'org-linkedin-wiki)

(setq org-todo-keywords
      '((sequence "TODO(t!)"
                  "WORKING(w!)" "PAUSED(p!)" "QUERY(q)" "TESTING(e!)"
                  "|" "SENT(s!)" "DONE(d!)" "LATER(l!)" "FAILED(f!)" "DELAYED(a!)" "CANCELED(c!)" "DUPLICATE(u!)")
	))

(setq org-todo-keyword-faces
      '(
        ("TODO"  . (:foreground "red" :weight bold))
        ("DELAYED"  . (:foreground "red" :weight bold))
        ("WORKING"  . (:foreground "gold2" :weight bold))
        ("PAUSED"  . (:foreground "gold2" :weight bold))
        ("TESTING"  . (:foreground "gold2" :weight bold))
        ("DONE"  . (:foreground "forestgreen" :weight bold))
        ("QUERY"  . (:foreground "darkred" :weight bold))
        ("SENT"  . (:foreground "SpringGreen4" :weight bold))
        ("LATER"  . (:foreground "dimgrey" :weight bold))
        ("FAILED"  . (:foreground "DarkOrange3" :weight bold))
        ("CANCELED"  . shadow)
        ("DUPLICATE"  . shadow)))

;; turn off _ meaning <sub> etc.  If I really want this feature
;; it's possible to do:
;; #+OPTIONS ^:nil
;; in an orgmode file.
(setq org-export-with-sub-superscripts nil)

;; Fix multi-line wrapping and what not in org mode
;;
;; I can't use this unless I upgrade emacs.  It crashes 23.1
;; (setq org-startup-indented nil)

;; Make it so long lines wrap in org mode.
;; This might be possible to remove if clean mode can be made to work.
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)

;; Make ctrl-return insert a subheading.
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key [(control return)] 'org-insert-subheading)

            ;; Is there a better way to do this?  I'm not sure why org-mode doesn't honor the
            ;; global settings.
            (define-key flyspell-mode-map (kbd "C-;") 'undo)
            (local-set-key [M-left]  'windmove-left)
            (local-set-key [M-right] 'windmove-right)
            (local-set-key [M-up]    'windmove-up)
            (local-set-key [M-down]  'windmove-down)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text Mode configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make it so long lines wrap even on internal frames (not windows)
(setq truncate-partial-width-windows nil)

;;
;; Other line wrapping goodies:
;; Visual Line mode.
;; Long line mode... apparently this mode sucks.
;; fill paragraph (realigns a paragraph based on the frame or fill column.)
;;
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LDAP and EMAIL command setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I have tabled this for the moment since it depends on ldapsearch
;; and that doesn't appeat to be installed right now :-p

;; (require 'eudc)

;; ;; make it work with tab in mail mode.
;; (eval-after-load
;;  "message"
;;  '(define-key message-mode-map [(control ?c) (tab)] 'eudc-expand-inline))
;; (eval-after-load
;;  "sendmail"
;;  '(define-key mail-mode-map [(control ?c) (tab)] 'eudc-expand-inline))

;; Reference linkedin's ldap server



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting up the Python IDE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Info additions
;;
(add-to-list 'load-path "~/emacs-init/contrib/pydoc-info-0.2/")
(require 'pydoc-info)

;;
;; Python mode stuff
;;
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(font-lock-add-keywords 'python-mode (font-lock-width-keyword 80))

;;
;; Autopair mode
;;
;; NOTE: wrong argument characterp issues are usually autopair's fault.
;;
(require 'autopair)

;; Don't add autopair in certain modes
(add-hook 'sldb-mode-hook #'(lambda ()
                              (setq autopair-dont-activate t)
                              (autopair-mode -1)))
(add-hook 'term-mode-hook #'(lambda ()
                              (setq autopair-dont-activate t)
                              (autopair-mode -1)))

;; enable autopair in all buffers
(autopair-global-mode)



;;;;;;;;;;;;;;;;;;;;;  Linkedin Specific Stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the indent level to be 2 spaces
(setq python-indent 2)
(setq python-indent-offset 2)

;; Remember that in python mode when you eval a region it only produces
;; the visible results of doing so.

;; Turn on flymake with pylink et al.

;; Move flymake errors to the mini-buffer
(require 'flymake-cursor)

;; Run the pycheck.sh script to make flymake work
;; with python files.
(add-hook 'find-file-hook 'flymake-find-file-hook)

(defun flymake-pycheck-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name temp-file
                                         (file-name-directory buffer-file-name))))
    (list "flake8" (list local-file))))

(when (load "flymake" t)
   (add-to-list 'flymake-allowed-file-name-masks
                '("\\.py\\'" flymake-pycheck-init)))

;; If we're in python mode, make sure flymake comes on.
(add-hook 'python-mode-hook #'(lambda ()
                                ;; enable flymake-python for files with no '.py' extension
                                (make-local-variable 'flymake-allowed-file-name-masks)
                                (add-to-list 'flymake-allowed-file-name-masks
                                             '("" flymake-pycheck-init))))

(global-set-key [f10] 'flymake-goto-prev-error)
(global-set-key [f11] 'flymake-goto-next-error)

;; Automatically delete trailing whitespace when saving files
;; while you are in python major mode.
;; Also, only use spaces, no tabs.
;; Also, turn on CamelCase navigation mode
(add-hook 'python-mode-hook
          (lambda()
            (setq-default indent-tabs-mode nil)
            (turn-on-subword-mode)
            (let ((match-all-regex "."))
              (add-to-list 'flymake-allowed-file-name-masks `(,match-all-regex flymake-pycheck-init))
              (flymake-mode)
              (setq flymake-allowed-file-name-masks
                    (remove-if (| (equalp match-all-regex (car %))) flymake-allowed-file-name-masks)))
            (flymake-mode)))

(add-hook 'before-save-hook (lambda (&optional foo) (delete-trailing-whitespace)))


;; Linkedin allows crazy long lines.
(font-lock-add-keywords 'python-mode (font-lock-width-keyword 120))

;; Activate pymacs (it should be installed in the system elisp code.)
;;
;; Pymacs documentation link: http://pymacs.progiciels-bpi.ca/pymacs.html
;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)

;; ;;If you plan to use a special directory to hold your own Pymacs code
;; ;;in Python, which should be searched prior to the usual Python import
;; ;;search path, then uncomment the next two lines:
;; ;;
;; ;;(eval-after-load "pymacs"
;; ;;     '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))
;; (require 'pymacs)
;; (pymacs-load "ropemacs" "rope-")
;; (setq ropemacs-enable-autoimport t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  SVN Stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(require 'psvn)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Hippie Expand ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For now I'm just going to set hippie expand to take over M-/, since
;; that should be similar to what I have now.
;; It looks like ac/yas will share the tab in a reasonable way, if I think
;; of a more efficient way to use it, I will.
(global-set-key (kbd "C-/") 'hippie-expand)
(global-set-key (kbd "M-/") 'dabbrev-expand)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Scala Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/emacs-init/contrib/scala-emacs")
(require 'scala-mode-auto)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Groovy Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;;; make Groovy mode electric by default.
;;; I turned this off because autopair is smarter, and seems to do everything
;;; electric mode does.
;;;
;; (add-hook 'groovy-mode-hook
;;           '(lambda ()
;;              ;; turn off autopair for this mode.
;;              (autopair-mode 0)
;;              (require 'groovy-electric)
;;              (groovy-electric-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Confluence Mode  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/emacs-init/contrib/confluence-el")
(require 'confluence)
(setq confluence-url "https://iwww.corp.linkedin.com/wiki/cf/rpc/xmlrpc")
(add-to-list 'auto-mode-alist '("\\.wiki\\'" . confluence-mode))
(put 'set-goal-column 'disabled nil)
