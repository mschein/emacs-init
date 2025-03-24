;; -*- lexical-binding: t -*-
;;
;; Copyright (C) by Michael Scheinholtz.  All Rights Reserved.
;;
;; Link to this file with: ln -s ~/emacs-init/emacs-init.el .emacs
;;
;; TODO:
;; 1. reorg doc here.
;; 2. Need to figure out tab and hippie expand to
;;    get everything to play nicely
;;
;; Widget library: https://www.gnu.org/software/emacs/manual/html_mono/widget.html
;;
;; To open a file with sudo do:
;; /sudo::
;;
;; find Jedi
;;
;; Edebug: [debug]
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Edebug.html#Edebug
;; To turn on edebug: C-u C-M-x (function definition)
;; also see: lispy-mode
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
;; Python development notes:
;;   - https://emacs.stackexchange.com/questions/9696/how-do-you-create-a-robust-python-ide-with-emacs-as-the-text-editor
;;
;; Rectangles
;; -
;;
;; xref stack
;; M-, xref-pop-marker-stack...
;;
;; calc notes:
;;  - m a  ; algebraic mode
;;  - m r  ; radian mode
;;  - https://www.gnu.org/software/emacs/manual/html_node/calc/Getting-Started.html#Getting-Started
;;  - C-x * q ; quick calc
;;  - C-x * c ; regular calc
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
;; C-x C-b (list-buffers.m)
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
;; M-x check-parens to find unbalanced stuff.
;;
;; Compile a common lisp function with debugging enabled.
;; : C-u C-c C-k
;;
;;
;; Fix font... I am not using the anti-aliased fonts.
;;
;; Global includes
;;
;; c-x c-k l:Turn the last 100 commands into a macro
;;
;; More emacs key strokes.
;;   http://www.cs.rutgers.edu/LCSR-Computing/some-docs/emacs-chart.html
;;
;; Mastering eshell:
;;   http://www.masteringemacs.org/articles/2010/12/13/complete-guide-mastering-eshell/
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
;;
;; Fix OSX Sierra Mini buffer issue:
;; -> System Preferences... → Dock → Prefer tabs when opening documents and select "Manually"
;; ;; -> This didn't actually work, but maybe it's related.
;;
;;
;; Another note about .elc files and byte-recompilation
;; (byte-recompile-directory "." 0)
;;  will force a recompilation... might want that in here.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Early Config values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We want all of our elisp under this repo.
(setq user-emacs-directory "~/emacs-init/")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup the package manager (Elpaca in this case.)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))


;;
;; Make it so we can install packages in the init file.
;;
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;;
;; When installing a package used in the init file itself,
;; e.g. a package which adds a use-package key word,
;; use the :wait recipe keyword to block until that package is installed/configured.
;; For example:
;;
;;(use-package general :ensure (:wait t) :demand t)
;; Expands to: (elpaca evil (use-package evil :demand t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile
  (require 'use-package))

;; Nice elisp enhancements
(use-package ht :ensure (:wait t) :demand t)
(use-package s :ensure (:wait t) :demand t)
(use-package f :ensure (:wait t) :demand t)

;; Packages Put them in dependency order
(use-package dockerfile-mode :ensure (:wait t) :demand t)
(use-package go-mode :ensure (:wait t) :demand t)
(use-package groovy-mode :ensure (:wait t) :demand t)
(use-package ini-mode :ensure (:wait t) :demand t)
(use-package jinja2-mode :ensure (:wait t) :demand t)
(use-package json-mode :ensure (:wait t) :demand t)
(use-package markdown-mode :ensure (:wait t) :demand t)
(use-package lsp-mode :ensure (:wait t) :demand t)
(use-package syslog-mode :ensure (:wait t) :demand t)
(use-package terraform-mode :ensure (:wait t) :demand t)
(use-package uuidgen :ensure (:wait t) :demand t)
(use-package yaml-mode :ensure (:wait t) :demand t)
(use-package scala-mode :ensure (:wait t) :demand t)
(use-package php-mode :ensure (:wait t) :demand t)
(use-package flymake-php :ensure (:wait t) :demand t)
(use-package package-lint :ensure (:wait t) :demand t)
(use-package cape :ensure (:wait t) :demand t)

;; Larger packages
(use-package transient :ensure (:wait t) :demand t)
(use-package magit :ensure (:wait t) :demand t)
(use-package yasnippet :ensure (:wait t) :demand t)
(use-package yasnippet-snippets :ensure (:wait t) :demand t)

;; For clojure dev
(use-package clojure-mode :ensure (:wait t) :demand t)
(use-package clojure-ts-mode :ensure (:wait t) :demand t)
(use-package cider :ensure (:wait t) :demand t)
(use-package clojure-essential-ref :ensure (:wait t) :demand t)

;; clojure-snippets?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs packages.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flyspell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package corfu
  :ensure (:wait t) :demand t

  ;; Optional customizations
  :custom
  (corfu-cycle t)                   ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup my personal packages.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Always load these.
(add-to-list 'load-path "~/emacs-init/mine/")
(dolist (path '("~/emacs-init/mine"
                "~/emacs-init/dynamic-modules"
                "~/emacs-init/contrib"))
  (when (file-directory-p path)
    (add-to-list 'load-path path)))

;; Turn on common lisp bindings
(require 'cl-lib)

;; TODO: XXXXXX
(require 'elisp-lib)
(require 'one-off-scripts)

;; Run our custom slime code
(require 'slime-lisp)

;; This is deprecated
(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup keyboard.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Do this first as it is unlikely to fail and makes it nicer to work with emacs.
;;
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

;; Make it so you can read my long buffer names
(setq Buffer-menu-name-width 50)

;; OSX key bindings
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char)) ;; sets fn-delete to be right-delet

;; Setup 'which-key' so we get a list of key binding options
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Emacs Config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Turn tabs off
(setq-default indent-tabs-mode nil)

;; Shut off useless menu items. etc.
(setq inhibit-splash-screen t)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Save desktops
(desktop-save-mode t)

;; Enable time in the mode line.
(display-time-mode t)

;; Flash the screen instead of making a noise on bell.
;; We'll see if I like this.
(setq visible-bell t)

;; automatically unzip/gunzip/uncompress files
(auto-compression-mode 1)

;; Make cut and paste work
(setq select-enable-clipboard t)

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
;; Make window navigation easier
;; Use shift and arrow keys
;;
(windmove-default-keybindings)

;; put all backup files in one place
(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.emacs-backups"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 8
      kept-old-versions 8
      version-control t)      ; use versioned backups

;; Make it so we can list timers if we want.
(put 'list-timers 'disabled nil)

;; Make it so sort-lines ignores case.
;; The debugger doesn't think this works, but it does as of emacs 30.
(setf sort-fold-case t)

;;
;; Add YASnippet support
;;
;; Note, it's the 'key' part that you should type.
;;
(require 'yasnippet) ;; not yasnippet-bundle
;; relocate my person extension dir
(setf yas-snippet-dirs (list (expand-file-name "~/emacs-init/snippets")
                             "~/emacs-init/elpaca/repos/yasnippet-snippets/snippets"))
(yas-global-mode 1)

;; We don't always need the final new line in a file.
(setq require-final-newline nil)

;; Allow these functions.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; If we can't exec stuff from /usr/local/bin
;; (setf exec-path (append exec-path '("/usr/local/bin")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Save Hook Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'before-save-hook #'delete-trailing-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Html Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'html-mode-hook
          (lambda ()
            ;; Default indentation is usually 2 spaces, changing to 4.
            (set (make-local-variable 'sgml-basic-offset) 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hippie Expand
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For now I'm just going to set hippie expand to take over M-/, since
;; that should be similar to what I have now.
;; It looks like ac/yas will share the tab in a reasonable way, if I think
;; of a more efficient way to use it, I will.
(global-set-key (kbd "C-/") 'hippie-expand)
(global-set-key (kbd "M-/") 'dabbrev-expand)

;;
;; This might be the way to go.
;;
(setq hippie-expand-try-functions-list
      (append hippie-expand-try-functions-list '(slime-complete-symbol)))
(setq smart-tab-completion-functions-alist
      '((emacs-lisp-mode . lisp-complete-symbol)
        (text-mode . dabbrev-completion)
        (slime-repl-mode . slime-complete-symbol)))


;; ;; pick a font.
;; ;; set fond size, etc. etc.
;; ;;(set-face-attribute 'default nil :height 110)
;; ;;
;; ;; hack can be downloaded from:
;; ;; https://sourcefoundry.org/hack/
;; ;;
;; ;; brew tap homebrew/cask-fonts
;; ;; brew cask install font-
;; ;;
;; ;; Interesting fonts:
;; ;; Hack
;; ;; Monaco
;; ;;
;; ;; To Try:
;; ;; Dejavu sans
;; ;;
;; ;;
;; ;;
;; (let ((font "Hack"))
;;   (when (member font  (font-family-list))
;;     (set-frame-font font t t)))
;; (set-face-attribute 'default nil :height 120)


;; ;; Enable ace-jump-mode
;; ;;
;; ;; use 'pop-mark' with it. (C-u C-<SPC>)
;; ;; global mark ring pop: (C-x C-<SPC>)
;; (require 'ace-jump-mode)
;; (define-key global-map (kbd "C-c C-<SPC>") 'ace-jump-mode)
;; (define-key global-map (kbd "C-x C-<SPC>") 'ace-jump-mode-pop-mark)


;; XXX Do we want this?
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Electic Auto Pair Stuff  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; Enable globally, but disable a few things.
;; ;;
;; (electric-pair-mode 1)

;; ;; Don't add autopair in certain modes
;; (dolist (mode '(sldb-mode-hook term-mode-hook shell-mode-hook))
;;   (add-hook mode (lambda ()
;;                    (electric-pair-local-mode -1))))


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


;; XXXX How did this work?
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Password Check  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-password-prompt-check "\\|^Enter host password .*:\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Perl Config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Get cperl-mode setup since it is better.
(defalias 'perl-mode 'cperl-mode)

(setq cperl-close-paren-offset -4)
(setq cperl-continued-statement-offset 4)
(setq cperl-indent-level 4)
(setq cperl-intent-parens-as-block t)
(setq cperl-tab-always-indent t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C Config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set a 4 space offset
(setq c-basic-offset 4)

;; Make it so C-n adds newlines.
(setq next-line-add-newlines t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ruby Config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ruby inside emacs
(use-package ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby")
(use-package inf-ruby
  :mode "\\.rb\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Php Config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'php-mode
  (add-hook 'php-mode-hook (lambda ()
                             (subword-mode 1)
                             (setq-local show-trailing-whitespace t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calc Mode Settings.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq calc-algebraic-mode t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Javascript Configuration
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-to-list 'load-path "~/emacs-init/javascript")
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.json\\'" . js-mode))


;; (autoload 'javascript-mode "javascript" nil t)

;; ;; flymake stuff is done later.
;; (let ((js-basic-offset 2))
;;   (setq js2-basic-offset js-basic-offset)
;;   (setq-default web-mode-markup-indent-offset js-basic-offset)
;;   (setq-default web-mode-css-indent-offset js-basic-offset)
;;   (setq-default web-mode-code-indent-offset js-basic-offset)
;;   (setq js-indent-level js-basic-offset)
;;   (setq sqml-basic-offset js-basic-offset))

;; ;; Enable flymake
;; (require 'flymake)
;; (if (< emacs-major-version 26)
;;     (progn
;;       (defmacro define-flymake-checker (func cmd &rest args)
;;         "Define a flymake checker function.
;;    It will be named `func', and will execute cmd + the rest of the args."

;;         (let ((temp-file (gensym))
;;               (local-file (gensym)))
;;           `(defun ,func ()
;;              (let* ((,temp-file (flymake-init-create-temp-buffer-copy
;;                                  #'flymake-create-temp-inplace))
;;                     (,local-file (file-relative-name ,temp-file
;;                                                      (file-name-directory buffer-file-name))))
;;                (list ,cmd (list ,@args ,local-file))))))
;;       (require 'flymake-cursor))
;;   ;; New flymake setup
;;   (eval-after-load 'flymake
;;     (progn
;;       (require 'flymake-diagnostic-at-point)
;;       (setq flymake-diagnostic-at-point-timer-delay 0.3)
;;       (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))))

;; ;;
;; ;; Turn on a jslint flymake
;; ;;
;; ;; to setup on a new osx system:
;; ;; - brew install npm
;; ;; - npm install -g jshint
;; ;; - npm install -g eslint eslint-plugin-react babel-eslint
;; ;;
;; ;; ln -s ~/emacs-init/dotfiles/eslintrc .eslintrc
;; ;;
;; ;; TODO: I should make a function to simplify these declarations.
;; ;;

;; (defun flymake-proc-jshint-init ()
;;   (let* ((temp-file (flymake-proc-init-create-temp-buffer-copy
;;                      'flymake-proc-create-temp-inplace))
;;          (local-file (file-relative-name
;;                       temp-file
;;                       (file-name-directory buffer-file-name))))
;;     (list "jshint" (list "--reporter=unix" local-file))))

;; (defun flymake-proc-eslist-init ()
;;   (let* ((temp-file (flymake-proc-init-create-temp-buffer-copy
;;                      'flymake-proc-create-temp-inplace))
;;          (local-file (file-relative-name
;;                       temp-file
;;                       (file-name-directory buffer-file-name))))
;;     (list "eslint" (list "-c" (expand-file-name "~/.eslintrc") "--no-color" "--format" "unix" local-file))))

;; (when (which "jshint")
;;   (if (< emacs-major-version 26)
;;       (progn
;;         (define-flymake-checker flymake-js-checker "jshint" "--reporter=unix")
;;         (define-flymake-checker flymake-eslint-checker "eslint" "-c" (expand-file-name "~/.eslintrc") "--no-color" "--format" "unix")

;;         (when (load "flymake" t)
;;           (add-to-list 'flymake-allowed-file-name-masks
;;                        '("\\.jsx\\'" flymake-eslint-checker))
;;           (add-to-list 'flymake-allowed-file-name-masks
;;                        '("\\.js\\'" flymake-js-checker)))
;;         ;;
;;         ;; Turn off flymake for xml/html since I can't get it to work
;;         ;;
;;         (setf flymake-allowed-file-name-masks (remove-if
;;                                                (| find (car %) '("\\.html?\\'" "\\.xml\\'" "\\.java\\'") :test #'equal)
;;                                                flymake-allowed-file-name-masks)))
;;     (progn
;;       (setq flymake-proc-allowed-file-name-masks
;;            (cons '("\\.js\\'"
;;                    flymake-proc-jshint-init
;;                    flymake-proc-simple-cleanup
;;                    flymake-proc-get-real-file-name)
;;                  flymake-proc-allowed-file-name-masks))
;;       (setq flymake-proc-err-line-patterns
;;            (cons '("^\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\): \\(.+\\)$"
;;                    1 2 3 4)
;;                  flymake-proc-err-line-patterns))

;;       (setq flymake-proc-allowed-file-name-masks
;;             (cons '("\\.jsx\\'"
;;                     flymake-proc-eslint-init
;;                     flymake-proc-simple-cleanup
;;                     flymake-proc-get-real-file-name)
;;                   flymake-proc-allowed-file-name-masks)))))

;; (defun js-type-hooks ()
;;   "Any commands we want to run when editing js style files (jsx etc.)"
;;   (subword-mode)
;;   (when (and (which "jshint")
;;              (>= emacs-major-version 26))
;;     (flymake-mode t)))

;; (add-hook 'js-mode-hook 'js-type-hooks)
;; (add-hook 'web-mode-hook 'js-type-hooks)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Shell Mode options
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Emacs now supports colors :-)
;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; ;; But it doesn't support less, so turn the pager off.
(setenv "GIT_PAGER" "")

;; ;;
;; ;; Turn on red highlighting for characters outside of the 80/100 char limit
;; (defun font-lock-width-keyword (width)
;;   "Return a font-lock style keyword for a string beyond width WIDTH
;; that uses 'font-lock-warning-face'."
;;   `((,(format "^%s\\(.+\\)" (make-string width ?.))
;;      (1 font-lock-warning-face t))))

;; (font-lock-add-keywords 'c++-mode (font-lock-width-keyword 80))
;; (font-lock-add-keywords 'python-mode (font-lock-width-keyword 99))
;; (font-lock-add-keywords 'java-mode (font-lock-width-keyword 100))
;; (add-hook 'java-mode 'subword-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aliases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Thanks Steve.
(defalias 'cr 'comment-region)
(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)
(defalias 'fnd 'find-name-dired)
(defalias 'i 'insert-interactive)
(defalias 'ic 'insert-interactive-completing-read)
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
(defalias 'pii 'python-insert-import-region)
(defalias 'ji 'jump-to-imports-python)
(defalias 'bu 'browse-url)
(defalias 'tf 'toggle-frame-fullscreen)
(defalias 'fow 'find-file-other-window)

(defalias 'shv 'open-shell-dir-venv)

;; More keyboard aliases
(global-set-key "\M-sb" 'multi-isearch-buffers)
(global-set-key "\M-sB" 'multi-isearch-buffers-regexp)

(global-set-key "\C-c\C-m" 'execute-extended-command) ;; alias for M-x

(global-set-key "\C-c\C-c" 'comment-region)
(global-set-key "\C-cu" 'uncomment-region)
(global-set-key "\M-p" 'backward-paragraph)
(global-set-key "\M-n" 'forward-paragraph)

(global-set-key "\C-cd" 'duplicate-line)

(global-set-key "\C-c\C-r" 'reload-file)
(global-set-key "\C-xt" 'open-todo)
(global-set-key (kbd "C-;") 'undo)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-x\M-f" 'find-file-at-point)


;;
;; NOTE: we could change how uniquify handles buffer name collisions if we want
;; https://pragmaticemacs.wordpress.com/2016/05/10/uniquify-your-buffer-names/
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flyspell, spelling code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun turn-on-flyspell ()
  "Force flyspell-mode on using a positive arg.  For use in hooks."
  (interactive)
  (flyspell-mode 1)
  (define-key flyspell-mode-map (kbd "C-;") 'undo))

(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'org-mode-hook 'turn-on-flyspell)
(add-hook 'html-mode-hook #'flyspell-prog-mode)
(add-hook 'python-mode-hook #'flyspell-prog-mode)
(add-hook 'php-mode-hook #'flyspell-prog-mode)
(add-hook 'ruby-mode-hook #'flyspell-prog-mode)
(add-hook 'perl-mode-hook #'flyspell-prog-mode)
(add-hook 'clojure-mode-hook #'flyspell-prog-mode)
(add-hook 'lisp-mode-hook #'flyspell-prog-mode)


;; ;; ;; Found on interwebs:
;; ;; ;; http://emacsblog.org/2007/03/12/tab-completion-everywhere/
;; ;; (defun indent-or-expand (arg)
;; ;;   "Either indent according to mode, or expand the word preceding
;; ;; point."
;; ;;   (interactive "*P")
;; ;;   (if (and
;; ;;        (or (bobp) (= ?w (char-syntax (char-before))))
;; ;;        (or (eobp) (not (= ?w (char-syntax (char-after))))))
;; ;;       (dabbrev-expand arg)
;; ;;     (indent-according-to-mode)))

;; ;; (defun my-tab-fix ()
;; ;;   (local-set-key [tab] 'indent-or-expand))

;; ;; (add-hook 'c-mode-hook          'my-tab-fix)
;; ;; (add-hook 'ruby-mode-hook       'my-tab-fix)
;; ;; (add-hook 'sh-mode-hook         'my-tab-fix)
;; ;; (add-hook 'emacs-lisp-mode-hook 'my-tab-fix)
;; ;; ;;(add-hook 'slime-mode-hook 'my-tab-fix)
;; ;; (add-hook 'clojure-mode-hook 'my-tab-fix)


;; ;; from http://joost.zeekat.nl/2010/06/03/slime-hints-3-interactive-completions-and-smart-tabs/


;; ;; Turn on auto completion.
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/emacs-init/contrib/ac-dict")
;; (ac-config-default)
;; (ac-flyspell-workaround)


;; ;; default to better frame titles
;; (setq frame-title-format
;;       (concat  "%b - emacs@" (system-name)))

;; ;; default to unified diffs
;; (setq diff-switches "-u")

;; ;;; uncomment for CJK utf-8 support for non-Asian users
;; ;; (require 'un-define)

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
;; C-c C-o (open a thing at point)
;;

(require 'org)
(require 'ox-html)
;; recommended key bindings from the manual.
;;C-c C-o  opens saved links
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-c\C-l" 'org-insert-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-c o" 'org-open-at-point-global)
(defalias 'osl 'org-store-link)

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

;;
;; Try to make it harder to invisibly mess up your org doc.
;;
(setq org-catch-invisible-edits 'smart)

;; turn off _ meaning <sub> etc.  If I really want this feature
;; it's possible to do:
;; #+OPTIONS ^:nil
;; in an orgmode file.
(setq org-export-with-sub-superscripts nil)

;; Make it so inserting a new header works the way
;; I'm used to.
(setq org-insert-heading-respect-content t)

;; Turn on syntax highlighting in code blocks.
(setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-confirm-babel-evaluate nil
      ;; Should I have this on... needs more thought.
      org-edit-src-content-indentation 0
      )

;; Fix multi-line wrapping and what not in org mode
;;
;; I can't use this unless I upgrade emacs.  It crashes 23.1
(setq org-startup-indented t)

;; Adapt indentation for all org-mode lines
(setq org-adapt-indentation t)

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
            (local-set-key [S-left] 'windmove-left)
            (local-set-key [S-right] 'windmove-right)
            (local-set-key [S-up] 'windmove-up)
            (local-set-key [S-down] 'windmove-down)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lisp and Elisp, Emacs Lisp mode configs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'emacs-lisp-mode-hook #'flymake-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common Lisp options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;
;; Maybe use smart parens instead: https://smartparens.readthedocs.io/en/latest/
;;

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




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit Options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit options
(add-hook 'magit-mode-hook 'magit-load-config-extensions)



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Setting up the Python IDE
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;
;; ;; Info additions
;; ;;
;; (add-to-list 'load-path "~/emacs-init/contrib/pydoc-info-0.2/")
;; (require 'pydoc-info)

;; ;;
;; ;; Python mode stuff
;; ;;
;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;; (add-to-list 'interpreter-mode-alist '("python" . python-mode))
;; (font-lock-add-keywords 'python-mode (font-lock-width-keyword 80))

;; ;;
;; ;; See docs here:
;; ;; https://github.com/emacs-lsp/lsp-mode
;; ;;
;; ;; TODO: switch to the new mode.
;; ;;

;; (use-package lsp-mode
;;   :hook (java-mode . lsp)
;;   :hook (ruby-mode . lsp)
;;   :hook (go-mode . lsp)
;;   :hook (python-mode . lsp)
;;   ;;
;;   ;; python mode gets setup below, to make sure
;;   ;; everything works with virtualenvs
;;   ;;
;;   :commands lsp)

;; (use-package lsp-ui :commands lsp-ui-mode)
;; (use-package company-lsp :commands company-lsp)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)
;; (use-package dap-mode)

;; ;;
;; ;; Python Mode stuff.
;; ;;

;; ;;
;; ;; Disable this for now, unless we go back to Google Style.
;; ;;
;; ;; ;; Set the indent level to be 2 spaces
;; ;; (setq python-indent 2)
;; ;; (setq python-indent-offset 2)

;; ;; Remember that in python mode when you eval a region it only produces
;; ;; the visible results of doing so.

;; ;; Turn on flymake with pylink et al.

;; (if (< emacs-major-version 26)
;;     (progn
;;       (define-flymake-checker flymake-flake8-checker "flake8")
;;       (define-flymake-checker flymake-flake83-checker "flake83")

;;       ;;
;;       ;; This is my old (25 and below) flymake setup code for Python.
;;       ;; for 26, python.el does what we need out of the box.
;;       ;;
;;       (defun setup-python-mode-common (interpreter checker)
;;         (setq py-python-command interpreter)
;;         (setq python-shell-interpreter interpreter)
;;         (update-flymake-mask "\\.py\\'" checker)
;;         (when (eql 'python-mode major-mode)
;;           (update-flymake-mask "." checker)))

;;       ;; (defun setup-python3-mode ()
;;       ;;   (interactive)
;;       ;;   (setup-python-mode-common "python3" #'flymake-flake83-checker)
;;       ;;   (message "Set python3 mode"))

;;       ;; (defun setup-python2-mode ()
;;       ;;   (interactive)
;;       ;;   (setup-python-mode-common "python" #'flymake-flake8-checker)
;;       ;;   (message "Set python2 mode"))


;;       ;; (when (load "flymake" t)
;;       ;;   ;; Do I need this, if I call this during the mode hook?
;;       ;;   (setup-python3-mode))

;;       ;; If we're in python mode, make sure flymake comes on.
;;       ;; (add-hook 'python-mode-hook (lambda ()
;;       ;;                               ;; enable flymake-python for files with no '.py' extension
;;       ;;                               (make-local-variable 'flymake-allowed-file-name-masks)
;;       ;;                               (if (guess-python-version-3)
;;       ;;                                   (setup-python3-mode)
;;       ;;                                 (setup-python2-mode))))
;;       )
;;   ;; emacs 26 version
;;   (progn
;;     (add-hook 'python-mode-hook (lambda ()
;;                                   ;; enable flymake-python for files with no '.py' extension
;;                                   (if (guess-python-version-3)
;;                                       (setq python-shell-interpreter "python3")
;;                                     (setq python-shell-interpreter "python"))))))

;; (global-set-key [f10] 'flymake-goto-prev-error)
;; (global-set-key [f11] 'flymake-goto-next-error)

;; (yapf-mode -1)

;; ;; Automatically delete trailing whitespace when saving files
;; ;; while you are in python major mode.
;; ;; Also, only use spaces, no tabs.
;; ;; Also, turn on CamelCase navigation mode
;; (add-hook 'python-mode-hook
;;           (lambda()
;;             (setq-default indent-tabs-mode nil)
;;             (subword-mode)
;;             (flymake-mode)
;;             ;; Setup lsp-mode, but in the right directory
;;             (destructuring-bind (pyls-path library-path)
;;                 (python-lsp-get-config)
;;               (set (make-variable-buffer-local 'lsp-pyls-server-command) pyls-path)
;;               ;; This call here causes problems sometimes.  Is this code actually needed?
;;               ;; or can i rely on lsp-pyls to set itself up.
;;               (set (make-variable-buffer-local 'lsp-clients-python-library-directories) (list library-path)))
;;             ;;
;;             ;; setup yapf mode.  Make sure we disabled it before we use our
;;             ;; special version
;;             ;;
;;             (yapf-mode -1)
;;             (lexical-let ((yapf-dir (file-name-directory (python-find-executable "yapf"))))
;;               (cl-flet ((python-before-save-hook ()
;;                           (message "start python save hook")
;;                           (whitespace-cleanup)
;;                           (message "start yapf %s" yapf-dir)
;;                           (with-exec-path yapf-dir
;;                             (message "yapf buffer %s" yapf-dir)
;;                             (yapfify-buffer))))
;;                 (add-hook 'before-save-hook #'python-before-save-hook nil t)))))

;; (add-hook 'before-save-hook #'delete-trailing-whitespace)

;; ;; Allows crazy long lines.
;; (font-lock-add-keywords 'python-mode (font-lock-width-keyword 120))

;; ;; Activate pymacs (it should be installed in the system elisp code.)
;; ;;
;; ;; Pymacs documentation link: http://pymacs.progiciels-bpi.ca/pymacs.html
;; ;; (autoload 'pymacs-apply "pymacs")
;; ;; (autoload 'pymacs-call "pymacs")
;; ;; (autoload 'pymacs-eval "pymacs" nil t)
;; ;; (autoload 'pymacs-exec "pymacs" nil t)
;; ;; (autoload 'pymacs-load "pymacs" nil t)

;; ;; ;;If you plan to use a special directory to hold your own Pymacs code
;; ;; ;;in Python, which should be searched prior to the usual Python import
;; ;; ;;search path, then uncomment the next two lines:
;; ;; ;;
;; ;; ;;(eval-after-load "pymacs"
;; ;; ;;     '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))
;; ;; (require 'pymacs)
;; ;; (pymacs-load "ropemacs" "rope-")
;; ;; (setq ropemacs-enable-autoimport t)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Confluence Mode  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; (add-to-list 'load-path "~/emacs-init/contrib/confluence-el")
;; ;; (require 'confluence)
;; ;; (add-to-list 'auto-mode-alist '("\\.wiki\\'" . confluence-mode))
;; ;; (put 'set-goal-column 'disabled nil)


;; ;; Get Hy
;; (add-to-list 'load-path "~/emacs-init/emacs-packages/hy-mode")
;; (require 'hy-mode)
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(custom-safe-themes
;;    '("80d5a22931c15756b00fb258b80c93b8bc5096bb698dadfb6155ef3550e1c8fb" default))
;;  '(dosbox-always-config '(nil))
;;  '(dosbox-exec "/Applications/dosbox/dosbox.app/Contents/MacOS/DOSBox")
;;  '(dosbox-games "/Users/mike/Documents/games/dos/")
;;  '(dosbox-global-config "~/Library/Preferences/DOSBox 0.74-3-3 Preferences")
;;  '(ignored-local-variable-values '((Base . 10) (Package . CL-USER) (Syntax . COMMON-LISP)))
;;  '(package-selected-packages
;;    '(org-magit magit transient terraform-mode company-lsp dracula-theme lsp-mode dap-mode lsp-java lsp-ui anaphora puppet-mode flymake-shellcheck flymake-python-pyflakes yapfify ryo-modal posframe flymake-diagnostic-at-point ini-mode ac-cider ac-emacs-eclim ac-html ac-slime company-jedi company-shell use-package hyperbole osx-browse osx-lib package pass password-store python-info svg ace-isearch ace-jump-mode closql smartparens yaml-mode s-buffer jinja2-mode daemons pipenv python-pytest magit-popup jira ldap-mode rdp sicp syslog-mode wget wolfram markdown-mode+ markdown-preview-mode macrostep dockerfile-mode auto-complete clojure-mode epl flycheck-perl6 flymake-go go-autocomplete go-guru go-mode go-playground go-snippets gotest json-mode let-alist perl6-mode pkg-info queue seq web-mode web-mode-edit-element which-key yasnippet google-this cider))
;;  '(sly-complete-symbol-function 'sly-simple-completions))

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Go Mode Settings  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'go-mode-hook (lambda ()
                          (setq tab-width 4)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Deal with anything in the local directory  ;;;;;;;;;;;;;;;;;;;;

;;
;; I used to load all files in the directory in ls order,
;; but I think it's better to just load a "local.el" file
;; and have it control how things are loaded.
;;
(let* ((local-only-dir "~/emacs-init/local")
       (local-only-code (path-join local-only-dir "local.el")))
  (when (file-directory-p local-only-dir)
    (add-to-list 'load-path local-only-dir))
  (when (file-exists-p local-only-code)
    (load local-only-code)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  JSON Parsing stuff  ;;;;;;;;;;;;;;;;;;;;
(require 'json)
