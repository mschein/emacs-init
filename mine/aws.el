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

(provide 'aws)
