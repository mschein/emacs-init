(require 'elisp-lib)

(defconst aws-subcommands
  '("acm"
    "apigateway"
    "application"
    "appstream"
    "athena"
    "autoscaling"
    "batch"
    "budgets"
    "clouddirectory"
    "cloudformation"
    "cloudfront"
    "cloudhsm"
    "cloudhsmv2"
    "cloudsearch"
    "cloudsearchdomain"
    "cloudtrail"
    "cloudwatch"
    "codebuild"
    "codecommit"
    "codepipeline"
    "codestar"
    "cognito-identity"
    "cognito"
    "cognito-sync"
    "cur"
    "datapipeline"
    "dax"
    "devicefarm"
    "directconnect"
    "discovery"
    "dms"
    "ds"
    "dynamodb"
    "dynamodbstreams"
    "ec2"
    "ecr"
    "ecs"
    "efs"
    "elasticache"
    "elasticbeanstalk"
    "elastictranscoder"
    "elb"
    "elbv2"
    "emr"
    "es"
    "events"
    "firehose"
    "gamelift"
    "glacier"
    "glue"
    "greengrass"
    "health"
    "iam"
    "importexport"
    "inspector"
    "iot"
    "iot"
    "kinesis"
    "kinesisanalytics"
    "kms"
    "lambda"
    "lex-models"
    "lex"
    "lightsail"
    "logs"
    "machinelearning"
    "marketplace"
    "marketplacecommerceanalytics"
    "meteringmarketplace"
    "mgh"
    "mobile"
    "mturk"
    "opsworks"
    "opsworkscm"
    "organizations"
    "pinpoint"
    "polly"
    "rds"
    "redshift"
    "rekognition"
    "resourcegroupstaggingapi"
    "route53"
    "route53domains"
    "sdb"
    "servicecatalog"
    "ses"
    "shield"
    "sms"
    "snowball"
    "sns"
    "sqs"
    "ssm"
    "stepfunctions"
    "storagegateway"
    "sts"
    "support"
    "swf"
    "waf"
    "waf"
    "workdocs"
    "workspaces"
    "xray"
    "s3api"
    "s3"
    "configure"
    "deploy"
    "configservice"
    "opsworks-cm")
  "A list of aws cli subcommands."
  )

;; Need to make this take do-cmd's arguments?
(defvar -aws-return-json nil "Control what the -aws function returns")

(defun* -aws (&rest args)
  (let ((resp (do-cmd (cons "aws" args) :stdout 'string :stderr 'string :throw t)))
    (when (equal (assoc1 :code resp) 0)
      (let ((json-result (ignore-errors
                           (json-read-from-string (assoc1 :stdout resp)))))
        (if -aws-return-json
            json-result
          (cons (cons :json json-result)
                resp))))))

(defun aws-define-aws-function-fn (name)
  (let ((args (gensym))
        (fn-name (format "aws-%s" name)))
    (message "Defining aws function %s" fn-name)
    `(defun ,(intern fn-name) (&rest ,args)
       ,(format "A function for accessing the aws %s command." name)
       (apply #'-aws ,name ,args))))

(defmacro aws-define-aws-function (&rest args)
  (apply #'aws-define-aws-function-fn args))

(defmacro aws-define-aws-sub-commands ()
  `(progn
     ,@(mapcar #'aws-define-aws-function-fn aws-subcommands)))

(aws-define-aws-sub-commands)

(defun aws-ecr-login ()
  "Login to Amazon ECR based on the current environment."
  (aws-ecr "get-login" "--no-include-email"))

(defun aws-ecr-list-images (&rest args)
  "Return an array of all of the images ids specified by
   running the aws ecr list-images command"

  (let ((-aws-return-json t))
    (assoc1 'imageIds
            (apply #'aws-ecr "list-images" args))))

(defun aws-ecr-describe-repositories (&rest args)
  "Return an array/alist of repository information as returned by aws ecr
   describe-repositories."
  (let ((-aws-return-json t))
    (assoc1 'repositories
            (apply #'aws-ecr "describe-repositories" args))))

(defun aws--filter-alist-to-str (filter)
  (string-join
   (mapcar (fn ((name . values))
                   (format "Name=%s,Values=%s" name (string-join (to-list values) ",")))
           filter)
   ","))

(defun aws--query-alist-to-str (return-value query)
  (format "%s[*].{%s}" return-value
          (string-join
           (mapcar
            (fn ((output-name . result-name))
                (concat output-name ":" result-name))
            query)
           ",")))

(cl-defun aws-ec2-describe-snapshots (&key filter query snapshot-ids)
  (let ((-aws-return-json t)
        (args (list "describe-snapshots")))
    (when filter
      (setf args (concatenate 'list args
                              (list "--filter" (aws--filter-alist-to-str filter)))))

    (when query
      (setf args (concatenate 'list args
                              (list "--query" (aws--query-alist-to-str "Snapshots" query)))))
    (when snapshot-ids
      (setf args (concatenate 'list args '("--snapshot-ids") (to-list snapshot-ids))))

    (apply #'aws-ec2 args)))

(defun aws-ec2-snapshots-sorted (volume-id)
  (let ((res (aws-ec2-describe-snapshots
              :filter `(("volume-id" . ,volume-id))
              :query '(("id" . "SnapshotId") ("time" . "StartTime")))))
    (sort res (| time-less-p
                 (safe-date-to-time (assoc1 'time %2))
                 (safe-date-to-time (assoc1 'time %1))))))

(defun data-to-buffer (data fmt &rest args)
  (to-buffer-switch
   (apply #'format fmt args)
   (pp-to-string data)))

(defun aws-latest-snapshot (volume-id)
  (interactive "svolume-id: ")
  (data-to-buffer (aref (aws-ec2-snapshots-sorted volume-id) 0)
                  "+latest-snapshot-%s-+" volume-id))

(defun aws-describe-snapshot (snapshot-id)
  (interactive "ssnapshot-id: ")
  (data-to-buffer
   (assoc1 'Snapshots
           (aws-ec2-describe-snapshots
            :snapshot-ids snapshot-id))
    "+-snapshot-%s-description+" snapshot-id))

(defun aws-describe-volume (volume-id)
  (interactive "svolume-id: ")
  (let ((-aws-return-json t))
    (data-to-buffer (aws-ec2 "describe-volumes"
                           "--filter" (format "Name=volume-id,Values=%s" volume-id))
                  "+volume-data-for-%s-+" volume-id)))

(defun aws-describe-volumes (volume-ids)
  (interactive "svolume-id: ")
  (let ((-aws-return-json t)
        (volume-ids (to-list volume-ids)))
    (data-to-buffer (apply #'aws-ec2 "describe-volumes" "--volume-ids" volume-ids)
                    "+volume-data-for-%s-+" (string-join volume-ids "-"))))

(defun aws-describe-instances (instance-ids)
  (interactive "sinstances: ")
  (let ((-aws-return-json t)
        (instance-ids (to-list instance-ids)))
    (data-to-buffer (apply #'aws-ec2 "describe-instances" "--instance-ids" instance-ids)
                    "+instance-data-for-%s-+" (string-join instance-ids "-"))))

(cl-defun aws-rds-describe-db-snapshots (&key filters snapshot-id db-id)
  (let ((-aws-return-json t)
        (args (list "describe-db-snapshots")))
    (when filters
      (setf args (concatenate 'list args
                              (list "--filter" (aws--filter-alist-to-str filters)))))

    (when snapshot-id
      (setf args (concatenate 'list args (list "--db-snapshot-identifier" snapshot-id))))

    (when db-id
      (setf args (concatenate 'list args (list "--db-instance-identifier" db-id))))

    (assoc1 'DBSnapshots (apply #'aws-rds args))))

(defun aws-rds-snapshots-sorted (db-id)
  (let ((time-symbol 'SnapshotCreateTime)
        (res (aws-rds-describe-db-snapshots :db-id db-id)))
    (sort res (| time-less-p
                 (safe-date-to-time (assoc1 time-symbol %2))
                 (safe-date-to-time (assoc1 time-symbol %1))))))

(defun aws-latest-rds-snapshot (db-id)
  "Return the latest rds snapshot for the given db id"
  (interactive "sdb-id: ")
  (data-to-buffer (aref (aws-ec2-snapshots-sorted volume-id) 0)
                  "+latest-rds-snapshot-%s-+" db-id))


(provide 'aws)
