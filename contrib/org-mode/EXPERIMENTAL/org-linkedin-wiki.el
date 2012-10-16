;;; org-linkedin-wiki.el --- backend for exporting org files to linkedin's wiki.
;;
;;; Commentary:
;;
;; org-linkedin-wiki.el lets you convert Org files to MoinMoin files using
;; the org-export.el experimental engine.
;;
;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'org-linkedin-wiki)
;;
;; You also need to fetch Org's git repository and add the EXPERIMENTAL/
;; directory in your load path.
;; 
;; Fetch Org's git repository:
;; 
;; ~$ cd ~/install/git/
;; ~$ git clone git://repo.or.cz/org-mode.git
;;
;; Put this in your .emacs.el:
;; 
;; (add-to-list 'load-path "~/install/git/org-mode/EXPERIMENTAL/")
;;
;; Export Org files to MoinMoin: M-x org-linkedin-wiki-export RET
;;
;;; Todo:
;; 
;; - handle radio links
;; - support caption and attributes in tables
;; - better handline of source code and examples
;; - handle inline HTML
;;
;;; Code:

(require 'org-export)

(defvar org-linkedin-wiki-emphasis-alist
  '(("*" "_%s_" nil)
    ("/" "_%s_" nil)
    ("_" "_%s_" nil)
    ("+" "_%s_" nil)
    ("=" "_%s_" nil))
  "The list of fontification expressions for MoinMoin.")

(defvar org-linkedin-wiki-export-table-table-style "")
(defvar org-linkedin-wiki-export-table-header-style "")
(defvar org-linkedin-wiki-export-table-cell-style "")

(defun org-linkedin-wiki-export ()
  "Export the current buffer to MoinMoin."
  (interactive)
  (setq org-export-current-backend 'linkedin-wiki)
  (org-export-set-backend "linkedin-wiki")
  ;; FIXME see the problem `org-linkedin-wiki-export-footnotes'
  ;; (add-hook 'org-export-preprocess-final-hook 'org-linkedin-wiki-export-footnotes)
  (add-hook 'org-export-preprocess-before-backend-specifics-hook
            'org-linkedin-wiki-export-src-example)
  (org-export-render)
  ;; (remove-hook 'org-export-preprocess-final-hook 'org-linkedin-wiki-export-footnotes)
  (remove-hook 'org-export-preprocess-before-backend-specifics-hook 
               'org-linkedin-wiki-export-src-example))

(defun org-linkedin-wiki-export-header ()
  "Export the header part."
  nil)
  ;; (let* ((p (org-combine-plists (org-infile-export-plist)
  ;;                               org-export-properties))
  ;; 	 (title (plist-get p :title))
  ;; 	 (author (plist-get p :author))
  ;; 	 (date (plist-get p :date))
  ;;        (level (plist-get p :headline-levels)))
  ;;   (insert (format "= %s by %s =\n\n" title author))
  ;;   (if (plist-get p :table-of-contents)
  ;;       (insert (format "<<TableOfContents(%s)>>\n" level)))))

(defun org-linkedin-wiki-export-first-lines (first-lines)
  "Export first lines."
  (insert (org-export-render-content first-lines) "\n")
  (goto-char (point-max)))

(defun org-linkedin-wiki-export-heading (section-properties)
  "Export MoinMoin heading"
  (let* ((p section-properties)
	 (h (plist-get p :heading))
	 (s (format "h%s." (plist-get p :level))))
    (insert (format "%s %s\n" s h))))

(defun org-linkedin-wiki-export-quote-verse-center ()
  "Export #+BEGIN_QUOTE/VERSE/CENTER environments."
  (let (rpl e)
    (while (re-search-forward "^[ \t]*ORG-\\([A-Z]+\\)-\\(START\\|END\\).*$" nil t)
      (setq e (if (equal (match-string 2) "END") "/" "")) 
      (setq rpl 
	    (cond ((equal (match-string 1) "BLOCKQUOTE") "blockquote>")
		  ((equal (match-string 1) "VERSE") "pre>")
		  ((equal (match-string 1) "CENTER") "center>")))
      (replace-match (concat "<" e rpl) t))))

(defun org-linkedin-wiki-export-fonts ()
  "Export fontification."
  (while (re-search-forward org-emph-re nil t)
    (let* ((emph (assoc (match-string 3) org-linkedin-wiki-emphasis-alist))
	   (beg (match-beginning 0))
	   (begs (match-string 1))
	   (end (match-end 0))
	   (ends (match-string 5))
	   (rpl (format (cadr emph) (match-string 4))))
      (delete-region beg end)
      (insert begs rpl ends))))

(defun org-linkedin-wiki-export-links ()
  "Replace Org links with DokiWiki links."
  ;; FIXME: This function could be more clever, of course.
  (while (re-search-forward org-bracket-link-analytic-regexp nil t)
    (cond ((and (equal (match-string 1) "file:")
		(save-match-data
		  (string-match (org-image-file-name-regexp) (match-string 3))))
	   (replace-match 
	    (concat "[[Image:" (file-name-nondirectory (match-string 3)) "]]")))
	  (t 
	   (replace-match 
	    (concat "[\\1\\3" (if (match-string 5) " \\5]" "]")))))))


;; FIXME this function should test whether [1] is really a footnote.
;; `org-footnote-normalize' should add properties to the normalized
;; footnotes so that we can recognize them.
(defun org-linkedin-wiki-export-footnotes ()
  "Export footnotes."
  (goto-char (point-min))
  (let (refpos rpl begnote begfullnote endnote)
    (while (re-search-forward "\[[0-9]+\]" nil t)
	(save-excursion
	  (save-match-data
	    (goto-char (point-max))
	    (search-backward (concat (match-string 0) " ") nil t)
	      (setq begfullnote (match-beginning 0))
	      (setq begnote (match-end 0))
	      (goto-char (match-end 0))
	      (re-search-forward "^\[[0-9]+\]\\|\\'" nil t)
	      (setq endnote (match-beginning 0))
	      (setq rpl (replace-regexp-in-string
			 "\n" " " (buffer-substring endnote begnote)))
	      (setq rpl (replace-regexp-in-string "[ \t]+$" "" rpl))
	      (delete-region begfullnote endnote)))
	(replace-match (concat "<ref>" rpl "</ref>")))))

(defun org-linkedin-wiki-export-src-example ()
  "Export #+BEGIN_EXAMPLE and #+BEGIN_SRC."
  (goto-char (point-min))
  (let (start env)
    (while (re-search-forward "^[ \t]*#\\+BEGIN_\\(EXAMPLE\\|SRC\\).*\n" nil t)
      (setq env (match-string 1))
      (replace-match "{code}\n")
      (setq start (point))
      (re-search-forward (concat "^[ \t]*#\\+END_" env ".*\n") nil t)
      (replace-match "{code}\n"))))

(defun org-linkedin-wiki-export-lists ()
  "Export lists to MoinMoin syntax."
  (while (re-search-forward (org-item-beginning-re) nil t)
    (move-beginning-of-line 1)
    (insert (org-list-to-generic 
	     (org-list-parse-list t)
	     (org-combine-plists
	      '(:splice nil 
			:ostart "" :oend ""
			:ustart "" :uend ""
			:dstart "" :dend ""
			:dtstart "" :dtend " "
			:istart (concat (make-string (* 2 (1+ depth)) ?  )
					(if (eq type 'unordered)
					    "* " "# "))
			:iend "\n"
			:icount nil
			:csep "\n"
			:cbon "[X]" :cboff "[ ]"
			:cbtrans "[-]"))))))


(defun org-linkedin-wiki-export-tables ()
  "Convert tables in the current buffer to mediawiki tables."
  (while (re-search-forward "^\\([ \t]*\\)|" nil t)
    (org-if-unprotected-at (1- (point))
      (org-table-align)
      (let* ((beg (org-table-begin))
             (end (org-table-end))
             (raw-table (buffer-substring beg end)) lines)
	(setq lines (org-split-string raw-table "\n"))
	(apply 'delete-region (list beg end))
	(when org-export-table-remove-special-lines
	  (setq lines (org-table-clean-before-export lines 'maybe-quoted)))
	(setq lines
	      (mapcar
	       (lambda(elem)
		 (or (and (string-match "[ \t]*|-+" elem) 'hline)
		     (org-split-string (org-trim elem) "|")))
	       lines))
	(insert (orgtbl-to-lw lines nil))))))

(defun orgtbl-to-lw (table params)
  "Convert TABLE into a mediawiki table."
  (let ((params2 (list
		  :tstart (concat "{| class=\"wikitable\" " 
				  org-linkedin-wiki-export-table-table-style)
		  :tend "|}\n"
		  :lstart "|-\n"
		  :lend ""
		  :sep "\n"
		  :fmt (concat "\| " org-linkedin-wiki-export-table-cell-style " | %s")
		  :hfmt (concat "! scope=row " org-linkedin-wiki-export-table-header-style " | %s")
		  :hlsep "\n"
		  )))
    (orgtbl-to-generic table (org-combine-plists params2 params))))

;; Various empty function for org-export.el to work:
(defun org-linkedin-wiki-export-footer () "")
(defun org-linkedin-wiki-export-section-beginning (section-properties) nil)
(defun org-linkedin-wiki-export-section-end (section-properties) nil)
(defun org-export-linkedin-wiki-preprocess (parameters)
  "Do extra work for MoinMoin export."
  nil)
(defun org-export-linkedin-wiki-preprocess (parameters)
  "Do extra work for MoinMoin export."
  nil)

(provide 'org-linkedin-wiki)
