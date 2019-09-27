;; -*- lexical-binding: t -*-

(require 'elisp-lib)

(defvar openssl-binary "/usr/bin/openssl")
(defvar cert-default-strength-bits 2048)

(defun openssl-is-installed ()
  (file-exists-p openssl-binary))

(assert (openssl-is-installed) nil "Please install openssl before using this module.
   Try brew or another package manager.")

(cl-defun openssl-create-private-key (output-path &key signed
                                                  (strength-bits cert-default-strength-bits))
  "Generate a private key .pem file."
  (apply #'run openssl-binary
         `("genrsa"
           ,@(when signed '("-aes256"))
           "-out" ,output-path
           ,(number-to-string strength-bits))))

;;
;; /C= 	Country             GB
;; /ST=	State 	            London
;; /L= 	Location            London
;; /O= 	Organization        Global Security
;; /OU=	Organizational Unit IT Department
;; /CN=	Common Name 	    example.com
;;


(defun subject-alist-to-subj (subject-alist)
  (let ((table '((:country . "C")
                 (:state . "ST")
                 (:location . "L")
                 (:organization . "O")
                 (:organizational-unit . "OU")
                 (:domain . "CN")
                 (:common-name . "CN"))))
    (concat "/"
            (string-join (cl-loop for (k . v) in subject-alist
                                  collect (concat (assoc1 k table) "=" v))
                         "/"))))

(cl-defun openssl-generate-csr (output-path private-key-file subject-alist)
  "Generate a certificate-signing-request with the given key"
  (run openssl-binary
       "req"
       "-new" "-key" private-key-file
       "-out" output-path
       "-subj" (subject-alist-to-subj subject-alist)))

(cl-defun openssl-sign-cert (output-path &key csr private-key (days 365))
  (run openssl-binary "x509"
       "-req"
       "-days" (number-to-string days)
       "-in" csr
       "-signkey" private-key
       "-out" output-path))

(cl-defun openssl-create-cert (output-path &key subject-alist
                                           (days 365)
                                           (strength-bits cert-default-strength-bits))
  (let* ((abs-output-path (file-truename output-path))
         (dir (file-name-directory abs-output-path))
         (cert (basename abs-output-path))
         (csr "sign-req.csr")
         (private-key "private-key.pem"))

    (pushd dir
      (message "Using dir %s" default-directory)

      ;; generate private key
      (openssl-create-private-key private-key  :strength-bits strength-bits)

      ;; generate csr
      (openssl-generate-csr csr private-key subject-alist)

      ;; sign cert.
      (openssl-sign-cert cert :csr csr :private-key private-key :days days)

      `((:private-key . ,(file-truename private-key))
        (:csr . ,(file-truename csr))
        (:cert . ,(file-truename cert))))))

(defun openssl-view-cert (cert-path)
  "View certificate details."
  (run-to-str openssl-binary "x509" "-text" "-noout" "-in" cert-path))

(provide 'openssl)
