;
;; Copyright (C) by Michael Scheinholtz.  All Rights Reserved.
;;
;; TODO: 
;; 1. reorg doc here.
;; 2. Need to figure out tab and hippie expand to
;;    get everything to play nicelyl
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
;;
;;M-x dirs will tell the shell buffer to figure out what the current working directory is.
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
;;
;; Fix font... I am not using the anti-aliased fonts.
;;
;; Global includes
(require 'cl)
(require 'find-lisp)

;; My stuff
(dolist (path '("~/.emacs.d/mike" "~/.emacs.d/contrib"))
  (add-to-list 'load-path path))
(require 'elisp-lib)
;(require 'clojure)
(require 'one-off-scripts)

;; To run a slime, uncomment and slime-lisp and start
;; a new emacs.
;(require 'slime-lisp)
;(require 'slime-clj)


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
(add-to-list 'load-path "~/.emacs.d/ruby-mode")
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(autoload 'ruby-mode "ruby-mode" "Major mode for ruby code")

;; Auto indentation
(add-hook 'ruby-mode-hook (lambda () (local-set-key "\r" 'newline-and-indent)))

;; Ruby inside emacs
(require 'inf-ruby)

;; Javascript for json editing.
(add-to-list 'load-path "~/.emacs.d/javascript")
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
(autoload 'javascript-mode "javascript" nil t)

;;
(put 'upcase-region 'disabled nil)

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
(font-lock-add-keywords 'python-mode (font-lock-width-keyword 80))

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
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet-0.6.1c")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/.emacs.d/plugins/yasnippet-0.6.1c/snippets")
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


(put 'downcase-region 'disabled nil)


;; Shut off useless menu items. etc.
(setq inhibit-splash-screen t)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; pick a font.
(set-face-attribute 'default nil :height 80)


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

(defalias 'sb 'multi-isearch-buffers)
(defalias 'sbx 'multi-isearch-buffers)

(defalias 'cr 'comment-region)
(defalias 'ucr 'uncomment-region)
(defalias 'shd 'shell-dir)
(defalias 'rlf 'reload-file)
(defalias 'sbx 'multi-isearch-buffers)

(global-set-key "\M-sb" 'multi-isearch-buffers)
(global-set-key "\M-sB" 'multi-isearch-buffers-regexp)

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key "\C-c\C-c" 'comment-region)
(global-set-key "\C-cu" 'uncomment-region)

(global-set-key "\M-p" 'backward-paragraph)
(global-set-key "\M-n" 'forward-paragraph)

(global-set-key "\C-cd" 'duplicate-line)

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))


;; put all backup files in one place
(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.emacs-backups"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 8
      kept-old-versions 2
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

(autoload 'python-mode "python-mode" "Python Mode." t)

;; Flyspell code
(defun turn-on-flyspell ()
   "Force flyspell-mode on using a positive arg.  For use in hooks."
   (interactive)
   (flyspell-mode 1))
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


;; (defun fix-font ()
;;   (interactive)
;;   (set-face-attribute 'default nil :height 100)
;;   ;;  (set-default-font "-adobe-courier-medium-r-normal--12-120-75-75-m-70-iso10646-1")
;;   ;;(set-default-font "Bitstream Vera Sans Mono-10")
;; ;;  (set-default-font "-bitstream-courier 10 pitch-medium-r-normal--0-0-0-0-m-0-adobe-standard")

;;   ;; (set-default-font "DejaVu Sans Mono-10")
;;   ;;(set-default-font "DejaVu Sans Mono-13")
;;   )

;; (fix-font)


;; Turn on auto completion.
(add-to-list 'load-path "~/.emacs.d/contrib/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/contrib/ac-dict")
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

(put 'downcase-region 'disabled nil)

;; Org mode customizations.
(setq org-todo-keywords
      '((sequence "TODO(t!)" "|" "WORKING(w)" "|" "DONE(d!)")
	(sequence "QUERY(q)" "|" "SENT(s)")
	(sequence "LATER(l)")
	(sequence "|" "FAILED(f!)")
	(sequence "|" "CANCELED(f!)")))

(setq org-todo-keyword-faces
      '(
        ("TODO"  . (:foreground "red" :weight bold))
        ("WORKING"  . (:foreground "gold2" :weight bold))
        ("DONE"  . (:foreground "forestgreen" :weight bold))
        ("QUERY"  . (:foreground "darkred" :weight bold))
        ("SENT"  . (:foreground "SpringGreen4" :weight bold))
        ("LATER"  . (:foreground "dimgrey" :weight bold))
        ("FAILED"  . (:foreground "DarkOrange3" :weight bold))
        ("CANCELED"  . shadow)))
