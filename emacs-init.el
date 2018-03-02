;;
;; Copyright (C) by Michael Scheinholtz.  All Rights Reserved.
;;
;; TODO:
;; 1. reorg doc here.
;; 2. Need to figure out tab and hippie expand to
;;    get everything to play nicely
;;
;; To open a file with sudo do:
;; /sudo::
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
;; M-x dirs will tell the shell buffer to figure out what the current working directory is.
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
;;Macro recording [permalink]
;;C-x ( : start recording keyboard macro
;;C-x ) : stop recording keyboard macro
;;C-x e : replay current keyboard macro
;;
;; Jumping around in a file
;; - use search
;; - registers
;;   - C-x r spc (letter) (save) spot under letter name
;;   - C-x r j (letter) (jump back to point)
;; - bookmarks
;;   - C-x r m (save named bookmark)
;;   - C-x r b (jump to a named bookmark)
;;   - C-x r l (list bookmarks)
;; - M-e and M-a to move by sentences.
;; - use c-u and M-- to vary counts.
;; - M-< and M-> go to start and end of files reliably.
;; - sexp movement
;;   - C-M-f C-M-b (forward and back by s-exp)
;; - M-m back to indent.... (a better C-a M-f)
;; - goto-line: M-g
;;
;; Rectangles
;; -
;;
;; xref stack
;; M-, xref-pop-marker-stack...
;;
;;
;; Browsing the kill-ring
;;  C-h v kill-ring (view the variable kill ring)
;;  C-y M-y to browse earlier kill-ring entries, or you can use C-u <num> C-y to dump
;;  earlier entries.
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
;;
;; to turn off annoying case issues, do:
;; M-x set-variable: case-fold-search to nil
;;
;; remember google supports g4
;;
;; M-x package-list-packages
;; or I think list-pacakages is better sometimes.
;;
;; eshell might be fun instead of shell
;;
;; find-* has useful file searching commands
;; dired is good for moving bunches of files.
;; Q in dired query replaces thigns in multiple files.
;; dired (wdired mode) writable dired mode: C-x C-q
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
;;
;; Recursively search files.
;; M-x find-name-dired: you will be prompted for a root directory and a filename pattern.
;; Press t to "toggle mark" for all files found.
;; Press Q for "Query-Replace in Files...": you will be prompted for query/substitution regexps.
;; Proceed as with query-replace-regexp: SPACE to replace and move to next match, n to skip a match, etc.
;;
;; find-grep is also useful for this.
;;
;; Magit, append a commit message with no changes.
;; C-u c
;; To do an ammended commit.
;; C-c C-a
;; lots of interesting features here... about chunks as well. I should use them more.
;;
;; full screen on osx:
;; Emacs 24.4
;;
;; Use ‘ns-use-native-fullscreen’ to get “native” fullscreen on OS X 10.7.
;;
;; Both M-x toggle-frame-fullscreen or M-x toggle-frame-maximized are available without having to define a function from below.
;;
;; yank:
;; M-y will browse previous entries.
;;
(require 'cl)
(require 'find-lisp)
(require 'midnight)
;; (require 's)
;; (require 'dash)
;; (require 'dash-functional)
;(require 'cssh)

;; My stuff
(dolist (path '("~/emacs-init/mine" "~/emacs-init/contrib" "~/emacs-init/contrib/groovymode" "~/emacs-init/elpa/http-post-simple-1.0"))
  (if (file-directory-p path)
      (add-to-list 'load-path path)))

;; Always load these.
(require 'elisp-lib)
(require 'one-off-scripts)

;; Add the new emacs package loader:


;; For Clojure work use nReple and ritz


;; To run a slime, uncomment and slime-lisp and start
;; a new emacs.
(require 'slime-lisp)

;; Add more info paths
(add-to-list 'Info-default-directory-list "/usr/local/share/info")

;; Note!  Use list-packages instead of package-list-packages.
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(setf package-user-dir "~/emacs-init/emacs-packages")

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

;; Setup 'which-key' so we get a list of key binding options
;; as we type.
(require 'which-key)
(which-key-mode)

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

;; OSX key bindings
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char)) ;; sets fn-delete to be right-delete

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
;; XXX May want to change how I do this.
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

;; Make it so C-n adds newlines.
(setq next-line-add-newlines t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javascript Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/emacs-init/javascript")
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

(autoload 'javascript-mode "javascript" nil t)

(defun js-type-hooks ()
  "Any commands we want to run when editing js style files (jsx etc.)"
  (subword-mode))

(add-hook 'js-mode-hook 'js-type-hooks)
(add-hook 'web-mode-hook 'js-type-hooks)

;; flymake stuff is done later.
(let ((js-basic-offset 2))
  (setq js2-basic-offset js-basic-offset)
  (setq-default web-mode-markup-indent-offset js-basic-offset)
  (setq-default web-mode-css-indent-offset js-basic-offset)
  (setq-default web-mode-code-indent-offset js-basic-offset)
  (setq js-indent-level js-basic-offset)
  (setq sqml-basic-offset js-basic-offset))


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
(add-hook 'java-mode 'subword-mode)

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
;; Note, it's the 'key' part that you should type.
;;
(require 'yasnippet) ;; not yasnippet-bundle
;; relocate my person extension dir
(setf yas-snippet-dirs (remove-if (| when (stringp %)
                                     (cl-search ".emacs.d" %)) yas-snippet-dirs))
(push "~/emacs-init/snippets" yas-snippet-dirs)
(yas/initialize)


;; We don't always need the final new line in a file.
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
(defalias 'cr 'comment-region)
(defalias 'eb 'eval-buffer)
(defalias 'fnd 'find-name-dired)
(defalias 'i 'insert-interactive)
(defalias 'ia 'insert-author)
(defalias 'ir 'indent-region)
(defalias 'ml 'magit-log)
(defalias 'ms 'magit-status)
(defalias 'mff 'magit-find-file)
(defalias 'qr  'query-replace "A short cut for query-replace")
(defalias 'qrr 'query-replace-regexp "A short cut for query-replace-regexp")
(defalias 'rlf 'reload-file)
(defalias 'rr 'replace-regexp "A short cut for replace-regexp")
(defalias 'sb 'multi-occur-all)
(defalias 'sbx 'multi-occur-in-matching-buffers)

(defalias 'cr 'comment-region)
(defalias 'ucr 'uncomment-region)
(defalias 'scc 'slime-connect-clj)
(defalias 'shd 'shell-dir)
(defalias 'sl 'sort-lines)
(defalias 'ucr 'uncomment-region)
(defalias 'ul 'underline)
(defalias 'ytp 'yank-to-file-location-python)
(defalias 'ji 'jump-to-imports-python)
(defalias 'bu 'browse-url)
(defalias 'tf 'toggle-frame-fullscreen)

(global-set-key "\M-sb" 'multi-isearch-buffers)
(global-set-key "\M-sB" 'multi-isearch-buffers-regexp)

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key "\C-c\C-c" 'comment-region)
(global-set-key "\C-cu" 'uncomment-region)
(global-set-key "\M-p" 'backward-paragraph)
(global-set-key "\M-n" 'forward-paragraph)

(global-set-key "\C-cd" 'duplicate-line)

(global-set-key "\C-c\C-r" 'reload-file)
(global-set-key "\C-xt" 'open-todo)
(global-set-key "\C-c\M-f" 'find-file-at-point)
(global-set-key (kbd "C-;") 'undo)

;; Save desktops
(desktop-save-mode t)

;; Flash the screen instead of making a noise on bell.
;; We'll see if I like this.
(setq visible-bell t)

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

;; Flyspell code
(defun turn-on-flyspell ()
   "Force flyspell-mode on using a positive arg.  For use in hooks."
   (interactive)
   (flyspell-mode 1)
   (define-key flyspell-mode-map (kbd "C-;") 'undo))
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(add-hook 'text-mode-hook 'turn-on-flyspell)

(setf exec-path (append exec-path '("/usr/local/bin")))

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

(setq org-todo-keywords
      '((sequence "TODO(t!)"
                  "WORKING(w!)" "PAUSED(p!)" "QUERY(q)" "TESTING(e!)"
                  "|" "REVIEWING(r!)" "SENT(s!)" "DONE(d!)" "LATER(l!)" "FAILED(f!)" "DELAYED(a!)" "CANCELED(c!)" "DUPLICATE(u!)")
	))

(setq org-todo-keyword-faces
      '(("TODO"  . (:foreground "red" :weight bold))
        ("DELAYED"  . (:foreground "red" :weight bold))
        ("WORKING"  . (:foreground "gold2" :weight bold))
        ("PAUSED"  . (:foreground "gold2" :weight bold))
        ("TESTING"  . (:foreground "gold2" :weight bold))
        ("REVIEWING"  . (:foreground "forestgreen" :weight bold))
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

;; Make it so inserting a new header works the way
;; I'm used to.
(setq org-insert-heading-respect-content t)

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


;;
;; Python Mode stuff.
;;

;;
;; Disable this for now, unless we go back to Google Style.
;;
;; ;; Set the indent level to be 2 spaces
;; (setq python-indent 2)
;; (setq python-indent-offset 2)

;; Remember that in python mode when you eval a region it only produces
;; the visible results of doing so.

;; Turn on flymake with pylink et al.

;; Move flymake errors to the mini-buffer
(require 'flymake-cursor)

;; Run the pycheck.sh script to make flymake work
;; with python files.
(add-hook 'find-file-hook 'flymake-find-file-hook)

(defmacro define-flymake-checker (func cmd &rest args)
  "Define a flymake checker function.
   It will be named `func', and will execute cmd + the rest of the args."

  (let ((temp-file (gensym))
        (local-file (gensym)))
    `(defun ,func ()
       (let* ((,temp-file (flymake-init-create-temp-buffer-copy
                           #'flymake-create-temp-inplace))
              (,local-file (file-relative-name ,temp-file
                                               (file-name-directory buffer-file-name))))
         (list ,cmd (list ,@args ,local-file))))))

(define-flymake-checker flymake-flake8-checker "flake8")
(define-flymake-checker flymake-flake83-checker "flake83")

(defun setup-python-mode-common (interpreter checker)
  (setq py-python-command interpreter)
  (setq python-shell-interpreter interpreter)
  (update-flymake-mask "\\.py\\'" checker)
  (when (bound-and-true-p python-mode)
    (update-flymake-mask "." checker)))

(defun setup-python3-mode ()
  (interactive)
  (setup-python-mode-common "python3" #'flymake-flake83-checker))

(defun setup-python2-mode ()
  (interactive)
  (setup-python-mode-common "python" #'flymake-flake8-checker))

(when (load "flymake" t)
  ;; Do I need this, if I call this during the mode hook?
  (setup-python3-mode))

;; If we're in python mode, make sure flymake comes on.
(add-hook 'python-mode-hook #'(lambda ()
                                ;; enable flymake-python for files with no '.py' extension
                                (make-local-variable 'flymake-allowed-file-name-masks)
                                (if (guess-python-version-3)
                                    (setup-python3-mode)
                                  (setup-python2-mode))))

(global-set-key [f10] 'flymake-goto-prev-error)
(global-set-key [f11] 'flymake-goto-next-error)

;; Automatically delete trailing whitespace when saving files
;; while you are in python major mode.
;; Also, only use spaces, no tabs.
;; Also, turn on CamelCase navigation mode
(add-hook 'python-mode-hook
          (lambda()
            (setq-default indent-tabs-mode nil)
            (subword-mode)
            (flymake-mode)))

(add-hook 'before-save-hook (lambda (&optional foo) (delete-trailing-whitespace)))

;; Allows crazy long lines.
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

;;
;; Turn on a jslint flymake
;;
;; to setup on a new osx system:
;; - brew install npm
;; - npm install -g jshint
;; - npm install -g eslint eslint-plugin-react babel-eslint
;;
;; ln -s ~/emacs-init/dotfiles/eslintrc .eslintrc
;;
;; TODO: I should make a function to simplify these declarations.
;;
(define-flymake-checker flymake-js-checker "jshint" "--reporter=unix")
(define-flymake-checker flymake-eslint-checker "eslint" "-c" (expand-file-name "~/.eslintrc") "--no-color" "--format" "unix")

(when (load "flymake" t)
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.jsx\\'" flymake-eslint-checker))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.js\\'" flymake-js-checker)))


;;
;; Turn off flymake for xml/html since I can't get it to work
;;
(setf flymake-allowed-file-name-masks (remove-if
                                       (| find (car %) '("\\.html?\\'" "\\.xml\\'" "\\.java\\'") :test #'equal)
                                       flymake-allowed-file-name-masks))

(add-hook 'html-mode-hook
          (lambda ()
            ;; Default indentation is usually 2 spaces, changing to 4.
            (set (make-local-variable 'sgml-basic-offset) 4)))


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Confluence Mode  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-to-list 'load-path "~/emacs-init/contrib/confluence-el")
;; (require 'confluence)
;; (add-to-list 'auto-mode-alist '("\\.wiki\\'" . confluence-mode))
;; (put 'set-goal-column 'disabled nil)


;; Get Hy
(add-to-list 'load-path "~/emacs-init/emacs-packages/hy-mode")
(require 'hy-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ht jira ldap-mode paredit pg rdp sicp syslog-mode wget wolfram markdown-mode markdown-mode+ markdown-preview-mode macrostep dockerfile-mode async auto-complete clojure-mode dash epl f flycheck flycheck-perl6 flymake-go git-commit go-autocomplete go-guru go-mode go-playground go-snippets gotest json-mode let-alist magit-popup perl6-mode pkg-info popup queue s seq spinner web-mode web-mode-edit-element which-key with-editor yasnippet magit google-this cider))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Go Mode Settings  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'go-mode-hook (lambda ()
                          (setq tab-width 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Password Check  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-password-prompt-check "\\|^Enter host password .*:\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Turn on an equivalent of proced on OSX ;;;;;;;;;;;;;;;;;;;;;;;;

;; Emacs's proced doesn't work on OSX.
(require 'vkill)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Deal with anything in the local directory  ;;;;;;;;;;;;;;;;;;;;

;; This must be done at the end so the rest of the environment is setup.
(let ((local-dir "~/emacs-init/local"))
  (if (file-directory-p local-dir)
      (dolist (file-path (directory-files local-dir 'full ".el$"))
	(load file-path))))


;; (quickproject:make-project "~/src/me/cheddar" :depends-on '(#:iterate #:let-plus #:alexandria #:closer-mop #:anaphora #:rutilsx)
