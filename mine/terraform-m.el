(require 'cl-lib)
(require 'elisp-lib)


(make-bookmark terrform-m-doc "https://developer.hashicorp.com/terraform/language/resources/syntax")

(defconst +terraform-m-registry+ "https://registry.terraform.io")
(defconst +terraform-m-provider-list+ '("hashicorp" "google"))
(defconst +terraform-m-registry-hashicorp+ (path-join +terraform-m-registry+ "/providers/hashicorp/"))
(defconst +terraform-m-registry-aws+ (path-join +terraform-m-registry-hashicorp+ "aws/latest/docs"))
(defconst +terraform-m-registry-aws-resources+ (path-join +terraform-m-registry-aws+ "resources"))

(defun terraform-m--open-doc-for-resource (resource &optional arguments)
  (let* ((url (path-join +terraform-m-registry-aws-resources+ resource))
         (url (if arguments
                  (concat url "#argument-reference")
                url)))
    (browse-url url)))

(defun terraform-m-find-doc-for-resource ()
  (interactive)

  (terraform-m--open-doc-for-resource (find-thing-at-point "\"")))

(defun terraform-m-find-arg-doc-for-resource ()
  (interactive)

  (terraform-m--open-doc-for-resource (find-thing-at-point "\"") t))

;; Consider using thing-at-point?
(defun terraform-m-find-doc-at-point ()
  (interactive)
  (save-excursion
    (re-search-backward "resource \"\\([^\"]*\\)\" ")
    (terraform-m--open-doc-for-resource (match-string 1))))

(provide 'terraform-m)
