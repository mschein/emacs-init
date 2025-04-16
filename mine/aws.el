;; -*- lexical-binding: t -*-

(require 'elisp-lib)
(require 'm-url-cache)

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
    "eks"
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
    "sso"
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

(defconst aws-region-table '((us-east-2 . ohio)
                             (us-east-1 . n-virginia)
                             (us-west-1 . n-california)
                             (us-west-2 . oregon)
                             (af-south-1 . cape-town)
                             (ap-east-1 . hong-kong)
                             (ap-south-1 . mumbai)
                             (ap-northeast-3 . osaka-local)
                             (ap-northeast-2 . seoul)
                             (ap-southeast-1 . singapore)
                             (ap-southeast-2 . sydney)
                             (ap-northeast-1 . tokyo)
                             (ca-central-1 . central)
                             (eu-central-1 . frankfurt)
                             (eu-west-1 . ireland)
                             (eu-west-2 . london)
                             (eu-south-1 . milan)
                             (eu-west-3 . paris)
                             (eu-north-1 . stockholm)
                             (me-south-1 . bahrain)
                             (sa-east-1 . sao-paulo)))

;; Need to make this take do-cmd's arguments?
(defvar -aws-return-json nil "Control what the -aws function returns")

;; We could hide this behind a dynamic variable
(m-url-cache-init)

;; TODO(mscheinh): get rid of this shim.
(defalias 'aws-traverse 'assoc1-traverse)

(defun* -aws (&rest args)
  (let ((resp (do-cmd (cons "aws" args) :stdout 'string :stderr 'string :throw t)))
    (when (do-cmd-succeeded-p resp)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tag Funcions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aws-ec2-individual-tags (tags)
  "`tags' is a list of cons cells, with car = key and cdr = value."
  (string-join (mapcar (lambda (tag)
                         (format "{Key=%s,Value=%s}" (car tag) (cdr tag)))
                       tags)
               ","))

(defun aws-ec2-make-tag-string (tag-list)
  "`tags' should be a list of:

   '((resource-type . <instance|volume|...>)
     (tags . ((<key> . <value>)))"

  (mapcar (lambda (tags)
            (format "ResourceType=%s,Tags=[%s]"
                    (assoc1 'resource-type tags)
                    (aws-ec2-individual-tags (assoc1 'tags tags))))
          tag-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ECR Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aws-ecr-get-login-password (&optional region)
  "Get the ecr login password"
  (apply #'aws-ecr "get-login-password" `(,@(if region (list "--region" region)))))

(defun aws-ecr-list-images (&rest args)
  "Return an array of all of the images ids specified by
   running the aws ecr list-images command"

  (let ((-aws-return-json t))
    (assoc1 'imageIds
            (apply #'aws-ecr "list-images" args))))

(defun aws-ecr-describe-images (repository registry-id)
  (let ((-aws-return-json t))
    (aws-ecr "describe-images"
             "--repository-name" repository
             "--registry-id" registry-id
             "--query" "sort_by(imageDetails,& imagePushedAt)[*]"
             "--no-paginate")))

(defun aws-ecr-describe-repositories (&rest args)
  "Return an array/alist of repository information as returned by aws ecr
   describe-repositories."
  (let ((-aws-return-json t))
    (assoc1 'repositories
            (apply #'aws-ecr "describe-repositories" args))))

(defun aws-ecr-create-repository (name)
  (let ((-aws-return-json t))
    (aws-ecr "create-repository" "--repository-name" name)))

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

(defun aws-ec2-describe-snapshots-by-tag (tag-value)
  (aws-ec2-describe-snapshots
   :filter (list (cons "tag-value"  tag-value))))

(defun aws-ec2-snapshots-sorted (volume-id)
  (let ((res (aws-ec2-describe-snapshots
              :filter `(("volume-id" . ,volume-id))
              :query '(("id" . "SnapshotId") ("time" . "StartTime")))))
    (sort res (| time-less-p
                 (safe-date-to-time (assoc1 'time %2))
                 (safe-date-to-time (assoc1 'time %1))))))

(cl-defun aws-ec2-describe-key-pairs (&key key-names)
  (let ((-aws-return-json t))
    (assoc1 'KeyPairs (apply #'aws-ec2 "describe-key-pairs" (when key-names (list "--key-name" key-names))))))

(cl-defun aws-ec2-create-key-pair (name)
  (let ((-aws-return-json t))
    (aws-ec2 "create-key-pair" "--key-name" name)))

(cl-defun aws-ec2-delete-key-pair (&key key-name key-pair-id)
  (let ((-aws-return-json t)
        (arg-builder (make-argument-builder)))

    (funcall arg-builder :opt "--key-name" key-name)
    (funcall arg-builder :opt "--key-pair-id" key-pair-id)

    (when (y-or-n-p "Are you sure you want to delete %s" (or key-name key-pair-id))
      (apply #'aws-ec2 "delete-key-pair" (funcall arg-builder :cli-args)))))

;;(cl-defun aws-ec2-create-volume ())

;;
;; tags can be an alist with:
;;  (resource-type . value)
;;  (tags . ((key . value)
;;           (key . value))
;;
;;
(cl-defun aws-ec2-create-or-run-instance (&key image-id
                                               instance-type
                                               key-name
                                               security-group-ids
                                               volume-size-gb
                                               block-device-mappings
                                               subnet-id
                                               (count 1)
                                               associate-public-ip-address
                                               name
                                               instance-tags ;; alist
                                               )
  "`block-device-mappings' is a list of cons cells for key value pairs.

`instance-tags' is a list of cons cells for key value pairs for tags for
the instance.

See: https://docs.aws.amazon.com/cli/latest/reference/ec2/run-instances.html
"
  (let ((-aws-return-json t)
        (arg-builder (make-argument-builder)))

    (assert (xor (not volume-size-gb) (not block-device-mappings)) nil
            "Dont set both volume-size-gb and block-device-mappings at the same time.")

    ;; Is there a slicker way to do this?
    (cl-flet ((args (&rest args)
                    (apply arg-builder args)))
      (args :opt "--image-id" image-id)
      (args :opt "--instance-type" instance-type)
      (args :opt "--key-name" key-name)
      (args :opt "--security-group-ids" security-group-ids)
      (args :opt "--subnet-id" subnet-id)
      (args :opt "--count" (number-to-string count))
      (args :flag "--associate-public-ip-address" associate-public-ip-address)

      (when volume-size-gb
        ;; This is annoying.  The json serializer expects the keywords to be symbols
        ;; and won't accept it if they are strings.
        (args :opt "--block-device-mappings" (json-serialize `((DeviceName . "/dev/sda1")
                                                               (Ebs . ((DeleteOnTermination . t)
                                                                       (VolumeSize . ,volume-size-gb)
                                                                       (VolumeType . "gp2")
                                                                       (Encrypted . t)))))))
      (when block-device-mappings
        (args :opt "--block-device-mappings" (json-serialize block-device-mappings)))

      ;;
      ;; You can only specify one set of flags.
      ;;
      (when name
        (push (cons "Name" name) instance-tags))

      (when instance-tags
        (args :opt "--tag-specifications" (first (aws-ec2-make-tag-string `(((resource-type . "instance")
                                                                             (tags . ,instance-tags)))))))
      (apply #'aws-ec2 "run-instances"
             (args :cli-args)))))

(cl-defun aws-ec2-stop-instance (instance-ids)
  (when (y-or-n-p (format "Are you sure you want to stop %s? " instance-ids))
    (let ((-aws-return-json t))
      (aws-ec2 "stop-instances" "--instance-ids" instance-ids))))

(cl-defun aws-ec2-terminate-instance (instance-ids)
  (when (y-or-n-p (format "Are you sure you want to TERMIANTE %s? " instance-ids))
    (let ((-aws-return-json t))
      (aws-ec2 "terminate-instances" "--instance-ids" instance-ids))))

;; Replace with something like with-overwrite-buffer-pp
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

(defun aws-list-regions ()
  (let ((-aws-return-json t))
    (assoc1 'Regions (aws-ec2 "describe-regions"))))

(defun aws-list-region-names ()
  (cl-loop for region across (aws-list-regions)
           collect (assoc1 'RegionName region)))

;; XXX Fix filters vs filter elsewhere.
(cl-defun aws-ec2-describe-instances (&key instance-ids filters)
  (let ((-aws-return-json t)
        (args '()))
    (when instance-ids
      (append! args (list "--instance-ids" (string-join (to-list instance-ids) ","))))
    (when filters
      (append! args (list "--filter" (aws--filter-alist-to-str filters))))
    (apply #'aws-ec2 "describe-instances" args)))

;; Look for ami tags.
;; expand this as needed in the future.
(defun aws-ec2-describe-images (name)
  (let ((-aws-return-json t))
    (aws-ec2 "describe-images" "--filters" (format "Name=name,Values=%s" name))))

(defun aws-describe-instances (&rest args)
  (interactive "sinstances: ")
  (let ((-aws-return-json t))
    (data-to-buffer (apply #'aws-ec2-describe-instances args)
                    "+instance-data-for-%s-+" (string-join (mapcar #'pp-to-string args) "-"))))

(cl-defun aws-terminate-instances (instance-ids &key do-it)
  (aws-ec2 "terminate-instances" "--instance-ids" (string-join (to-list instance-ids) ",")
           (if do-it
               "--no-dry-run"
             "--dry-run")))

(cl-defun aws-autoscaling-terminate-instance (instance-id &key do-it)
  "Terminate an instance in an autoscaling group.

   This is faster than regular terminate-instances, because we inform the asg
   so it can be smart.
  "
  (if do-it
      (aws-autoscaling "terminate-instance-in-auto-scaling-group" "--instance-id" instance-id "--no-should-decrement-desired-capacity")
    (aws-terminate-instances instance-id)))

(defun aws-get-instance-ips (instance-id)
  (aws-traverse
   '(Reservations 0 Instances 0 PrivateIpAddress)
   (aws-ec2-describe-instances :instance-ids instance-id)))

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

;; This is super slow, so do it async
(defun aws-rds-describe-db-engine-versions (cb-fn)
  (do-cmd-async (list "aws" "rds" "describe-db-engine-versions")
                :callback-fn cb-fn))

;; Should make the paging more generic?
;; should I use a callback?
(defun aws-rds-describe-db-instances ()
  (let ((-aws-return-json t)
        (first-iteration t)
        (next-token nil)
        (output '()))
    (cl-flet ((get-db-instances ()
                 (let ((args (list "describe-db-instances" "--max-items" "50")))
                   (when next-token
                     (append! args (list "--starting-token" next-token)))
                   (apply #'aws-rds args))))

      (cl-loop while (or first-iteration next-token)
               for resp = (get-db-instances)
               do (let ((token (assoc-get 'NextToken resp)))
                    (message "got token: %s" token)
                    (cl-loop for db across (assoc1 'DBInstances resp)
                             do (push db output))
                    (setf first-iteration nil)
                    (setf next-token token))))
    output))

(cl-defun aws-rds-query-db-instances (&key db-instance-re)
  (cl-loop for db in (aws-rds-describe-db-instances)
           if (string-match db-instance-re (assoc1 'DBInstanceIdentifier db))
              collect db))


(defun aws-latest-rds-snapshot (db-id)
  "Return the latest rds snapshot for the given db id"
  (interactive "sdb-id: ")
  (data-to-buffer (aref (aws-rds-snapshots-sorted db-id) 0)
                  "+latest-rds-snapshot-%s-+" db-id))

(defun aws-current-access-key-id ()
  (getenv "AWS_ACCESS_KEY_ID"))

(defun aws-ecs-list-clusters ()
  "Return a list of ecs clusters in the current account."
  (let ((-aws-return-json t)
        (aws-access-key (aws-current-access-key-id)))

    (assert aws-access-key)

    (let* ((clusters (m-url-cache-or-fetch
                      (format "awsenv:%s" aws-access-key)
                      (lambda () (assoc1 'clusterArns
                                         (aws-ecs "list-clusters")))
                      :ttl-sec (* 60 60 24 7)))
           ;; put any "default" clusters
           (default-index (search-substring "default" clusters)))

      ;; Put default and bitbucket up front.
      (when default-index
        (array-swap 0 default-index clusters))
      clusters)))

(defun aws-ecs-list-services (cluster)
  "This the service arns in a given `cluster'"
  (let ((-aws-return-json t))
    (assoc1 'serviceArns (aws-ecs "list-services" "--cluster" cluster))))
;; aws ecs describe-services --cluster "arn:aws:ecs:us-east-1:747953286079:cluster/bitbucket-backend-cluster-HYV7SDCTBJV3" --services "bitbucket-backend"

(defun aws-ecs-describe-services (cluster services)
  "Provide information about a list of `services' in an ecs `cluster'"
  (let ((-aws-return-json t))
    (assoc1 'services (apply #'aws-ecs "describe-services" "--cluster" cluster "--services" (to-list services)))))

(defun aws-ecs-list-tasks (cluster service-name)
  "Get a list of tasks for a given `service-name' in a `cluster'"
  (let ((-aws-return-json t))
    (assoc1 'taskArns (aws-ecs "list-tasks" "--cluster" cluster "--service-name" service-name))))

(defun aws-ecs-describe-tasks (cluster tasks)
  (let ((-aws-return-json t))
    (assoc1 'tasks (apply #'aws-ecs "describe-tasks" "--cluster" cluster "--tasks" (to-list tasks)))))

(defun aws-ecs-describe-task-definition (task-definition)
  (let ((-aws-return-json t))
    (assoc1 'taskDefinition (aws-ecs "describe-task-definition" "--task-definition" task-definition))))

(defun aws-ecs-describe-container-instances (cluster container-instances)
  (let ((-aws-return-json t))
    (assoc1 'containerInstances (apply #'aws-ecs
                                       "describe-container-instances" "--cluster" cluster "--container-instances"
                                       (to-list container-instances)))))

(defun array-swap (first second array)
  (let ((tmp (aref array first)))
    (aset array first (aref array second))
    (aset array second tmp)))

(defun search-substring (re seq)
  "Search a sequence for "
  (search (list re) seq :test #'string-match))

(defun aws-task->instance (cluster task)
  (aws-traverse '(0 ec2InstanceId)
                (aws-ecs-describe-container-instances cluster
                 (aws-traverse '(0 containerInstanceArn) (aws-ecs-describe-tasks cluster task)))))

(defun aws-task->ip (cluster task)
  (aws-get-instance-ips (aws-task->instance cluster task)))

(defun aws-ecs-get-ips-in-cluster (service cluster)
  (let ((-aws-return-json t))
    (mapcar (| aws-task->ip cluster %) (aws-ecs-list-tasks cluster service))))

(defun aws-get-task-definition-in-cluster (cluster service-name)
  (aws-ecs-describe-task-definition
       (aws-traverse '(0 taskDefinitionArn)
                     (aws-ecs-describe-tasks cluster (aref (aws-ecs-list-tasks cluster service-name) 0)))))

(defun aws-get-task-definition-for-service (service-name)
  (let ((-aws-return-json t))
    (let ((cluster (aws-find-service-cluster service-name)))
      (aws-get-task-definition-in-cluster cluster service-name))))

(defun aws-list-task-definitions (family-prefix)
  (let ((-aws-return-json t))
    (assoc1 'taskDefinitionArns
            (aws-ecs "list-task-definitions" "--family-prefix" family-prefix))))

(defun aws-latest-task-definition (family-prefix)
  (when-let ((task-defs (aws-list-task-definitions family-prefix)))
    (aref task-defs (1- (length task-defs)))))

(cl-defun aws-run-task (task-definition &key cluster cmd container)
  (when cmd
    (assert container))

  (assert cluster)

  (let ((args (list "--task-definition" task-definition)))
    (when cluster
      (append! args (list "--cluster" cluster)))
    (let ((-aws-return-json t))
      (apply #'aws-ecs "run-task" args))))

(defun aws-get-service-container (service-name)
  (aws-traverse '(containerDefinitions 0)
                (aws-get-task-definition-for-service service-name)))

(defun aws-get-service-ecr-image (service-name)
  (assoc1 'image (aws-get-service-container service-name)))

(defun aws-get-container-for-service (cluster service)
  (let ((-aws-return-json t))
    (assoc1 'containerDefinitions aws-get-task-definition-for-service)))

(defun aws-ecs-get-ips (service-name)
  (let ((-aws-return-json t))
    ;;
    ;; algorithm
    ;;
    ;; 1. list clusters in the environment
    ;; 2. search for the service in the default cluster.
    ;; 3. search in the bitbucket cluster.
    ;; 4. then try all of them.
    ;;

      ;; clusters should be ready now
      (cl-loop for cluster across (aws-ecs-list-clusters) do
         (progn
           (message "Check cluster %s for service %s" cluster service-name)
           (when-let (ips (with-demoted-errors "Check cluster for task: %S"
                            (aws-ecs-get-ips-in-cluster service-name cluster)))
             (return ips))))))

(defun aws-find-service-cluster (service-name)
  (cl-loop for cluster across (aws-ecs-list-clusters) do
           (progn
             (message "Check cluster %s for service %s" cluster service-name)
             (when (with-demoted-errors "Check cluster for task: %S"
                     (aws-ecs-list-tasks cluster service-name))
               (return cluster)))))

(cl-defun aws-ecs-service-pattern-in-env-p (service-pattern)
  (let ((aws-access-key (aws-current-access-key-id)))
    (assert aws-access-key)

    (cl-loop for cluster across (aws-ecs-list-clusters) do
             (cl-loop for service across (m-url-cache-or-fetch (format "env-cluster-services:%s-%s"  aws-access-key cluster)
                                                               (lambda () (aws-ecs-list-services cluster))
                                                               ;; cache for 5 min?
                                                               :ttl-sec (* 60 60 5))
                      when (string-match-p service-pattern service) do
                        (return-from aws-ecs-service-pattern-in-env-p cluster)))))

(defun aws-find-task-for-service (service-name)
  (let ((cluster (aws-find-service-cluster service-name)))
    (assert cluster)
    (aws-ecs-list-tasks cluster service-name)))


(defun aws-find-task-cluster (task-name)
  )
(defun aws-find-task (task-name)
  )

(cl-defun aws-ecs-collect-services (service-pattern)
  (let ((aws-access-key (aws-current-access-key-id)))
    (assert aws-access-key)

    (cl-loop for cluster across (aws-ecs-list-clusters) append
             (cl-loop for service across (m-url-cache-or-fetch (format "env-cluster-services:%s-%s"  aws-access-key cluster)
                                                               (lambda () (aws-ecs-list-services cluster))
                                                               ;; cache for 5 min?
                                                               :ttl-sec (* 60 60 5))
                      when (string-match-p service-pattern service)
                        collect (cons service cluster)))))

(cl-defun aws-ecs-service-in-env-p (service-name)
  (aws-ecs-service-pattern-in-env-p (format "service/%s$" service-name)))

;; TODO(mls): make this return a list.
(defun aws-ec2-lookup-instance-info (tag-value)
  ;;(aws-traverse '(Reservations 0 Instances 0)
  (cl-loop for instances across (assoc1 'Reservations
                                        (aws-ec2-describe-instances :filters `((tag-value . ,tag-value))))
           ;; Convert to a list.
           append (mapcar #'identity (assoc1 'Instances instances))))


(defun aws-ec2-lookup-instances (tag-value)
  (mapcar (| assoc1 'InstanceId %)
          (aws-ec2-lookup-instance-info tag-value)))

(defun aws-ec2-lookup-ips (tag-value)
  (mapcar #'aws-get-instance-ips (aws-ec2-lookup-instances tag-value)))

(defun aws-cluster-short-name (cluster)
  (let ((cluster (second (string-split "/" cluster))))
    (assert cluster)
    cluster))

(defun aws-list-account-aliases ()
  (aws-iam "list-account-aliases"))

(cl-defun aws-s3-get-object (bucket key output-file &key etag)
  (let ((args (list "aws" "s3api" "get-object" "--bucket" bucket "--key" key)))
    (when etag
      (append! args (list "--if-none-match" etag)))
    (append-atom! args output-file)

    (when-let ((res (do-cmd args :stdout 'string :stderr 'string)))
      (cons (cons :json (ignore-errors (json-read-from-string (assoc1 :stdout res))))
            res))))

(defun aws-s3-bucket-exists-p (bucket)
  (ignore-errors
    (do-cmd-succeeded-p (aws-s3api "head-bucket" "--bucket" bucket))))

(defun aws-describe-service (service)
  (aws-ecs-describe-services (aws-find-service-cluster service) service))

(defun aws-ssm-delete-parameter (name)
  (aws-ssm "delete-parameter" "--name" name))

(defun aws-lambda-exists (name)
  (do-cmd-succeeded-p (do-cmd (list "aws" "lambda" "get-function" "--function-name" name))))

(defun aws-lambda-get (name)
  (let ((-aws-return-json t))
    (aws-lambda "get-function" "--function-name" name)))

(defun aws-lambda-delete (name)
  (let ((-aws-return-json t))
    (aws-lambda "delete-function" "--function-name" name)))

;; Fix this name
(defun aws-firehose-describe-stream (name)
  (let ((-aws-return-json t))
    (aws-firehose "describe-delivery-stream" "--delivery-stream-name" name)))

(defun aws-kinesis-list ()
  "Return a list of kinesis stream names, be aware this doesn't deal with
paging yet."
  (let ((-aws-return-json t))
    (vector-to-list (assoc1 'StreamNames
                            (aws-kinesis "list-streams")))))

(defun aws-firehose-list ()
  "Return a list of firehose delivery stream names, be aware this
doesn't deal with paging yet."
  (let ((-aws-return-json t))
    (vector-to-list (assoc1 'DeliveryStreamNames
                            (aws-firehose "list-delivery-streams")))))

(defun aws-kinesis-describe-stream (stream-name)
  (let ((-aws-return-json t))
    (aws-kinesis "describe-stream" "--stream-name" stream-name)))

(defun aws-kinesis-stream-is-encrypted-p (stream-name)
  (not (equal "NONE" (assoc1 '(StreamDescription EncryptionType)
                             (aws-kinesis-describe-stream stream-name)))))

(defun aws-firehose-stream-is-encrypted-p (stream-name)
  (equal "ENABLED" (assoc1 '(DeliveryStreamDescription
                             DeliveryStreamEncryptionConfiguration
                             Status)
                           (aws-firehose-describe-stream stream-name))))

(cl-defun aws-kms-create-grant (&key region key-id grantee-principal operations-list)
  (assert (listp operations-list))
  (apply #'aws-kms
         "create-grant"
         "--region" region
         "--key-id" key-id
         "--grantee-principal" grantee-principal
         "--operations"
         operations-list))

(defun aws-kms-list-grants (key-id)
  (let ((-aws-return-json t))
    ;; I should add an decorator to do pagination.
    ;;
    ;; it appears paginate and --no-paginate are reversed for this command?
    ;;
    (assoc1 'Grants (aws-kms "list-grants" "--key-id" key-id))))

(cl-defun aws-kms-get-key-policy (key-id &key (policy-name "default"))
  (let ((-aws-return-json t))
    (assoc1 'Policy (aws-kms "get-key-policy" "--key-id" key-id "--policy-name" policy-name))))

(defun aws-acm-get-certificate (arn)
  (let ((-aws-return-json t))
    (aws-acm "get-certificate" "--certificate-arn" arn)))

(defun aws-acm-list-certificates ()
  (let ((-aws-return-json t))
    (assoc1 'CertificateSummaryList
            (aws-acm "list-certificates"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Iam and access functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aws-iam-list-roles ()
  (let ((-aws-return-json t))
    (assoc1 'Roles (aws-iam "list-roles"))))

(defun aws-iam-get-role (role-name)
  (let ((-aws-return-json t))
    (aws-iam "get-role" "--role-name" role-name)))

(defun aws-iam-role-exists-p (role-name)
  (condition-case err
      (not (not (aws-iam-get-role role-name)))
    (error
     (unless (string-match "NoSuchEntity" (error-message-string err))
       (error err)))))

(defun aws-assume-role (role-arn role-session-name)
  (let ((-aws-return-json t))
    (aws-sts "assume-role" "--role-arn" role-arn "--role-session-name" role-session-name)))

(defun aws-sts-get-caller-identity ()
  (aws-sts "get-caller-identity"))

(defun aws-has-credentials-no-cache-p ()
  (not (not
        (ignore-errors
          (aws-sts-get-caller-identity)))))

(defun aws-has-credentials-p ()
  (let ((time-keeping-file (expand-file-name "~/.aws-cred-time-keeper"))
        (credential-timeout (seconds-to-time (* 4 60))))
    (let ((result (if (time-less-p (time-add (if (file-exists-p time-keeping-file)
                                                 (get-file-modification-time time-keeping-file)
                                               0)
                                             credential-timeout)
                                   (current-time))
                      (aws-has-credentials-no-cache-p)
                    t)))
      (when result
        (touch time-keeping-file))
      result)))

(defun aws-elasticsearch-list-domain-names ()
  (let ((-aws-return-json t))
    (mapcar (fn (data)
              (assoc1 'DomainName data))
            (assoc1 'DomainNames (aws-es "list-domain-names")))))

(defun aws-elasticsearch-describe (domain-name)
  (let ((-aws-return-json t))
    (aws-es "describe-elasticsearch-domain" "--domain-name" domain-name)))

(defun aws-elasticsearch-get-endpoint (domain-name)
  (let ((ds  (assoc1 'DomainStatus (aws-elasticsearch-describe domain-name))))
    (if-let (end-point (assoc-get 'Endpoint ds))
        end-point
      (assoc1 '(Endpoints vpc) ds))))


(defun aws-elasticsearch-is-encrypted-p (domain-name)
  (not (eql :json-false
            (assoc1 '(DomainStatus EncryptionAtRestOptions Enabled)
                    (aws-elasticsearch-describe domain-name)))))

(defun aws-sqs-attributes (queue-url)
  (let ((-aws-return-json t))
    (assoc1 'Attributes
            (aws-sqs "get-queue-attributes" "--queue-url" queue-url "--attribute-names" "All"))))

(defun aws-sqs-list-queues (&optional prefix)
  (let ((-aws-return-json t))
    (assoc-get 'QueueUrls
               (apply #'aws-sqs `("list-queues"
                                  ,@(when prefix
                                      (list "--queue-name-prefix" prefix))))
               [])))

(defun aws-list-groups ()
  (let ((-aws-return-json t))
    (assoc1 'Groups
            (aws-iam "list-groups"))))

(defun aws-cloudfront-get-distribution (id)
  (let ((-aws-return-json t))
    (assoc1 'Distribution
            (aws-cloudfront "get-distribution" "--id" id))))

(defun aws-eks-describe-cluster (name)
  "Return info about the eks cluster"
  (let ((-aws-return-json t))
    (aws-eks "describe-cluster" "--name" name)))

(defun aws-eks-kubectl-info (name)
  "Get the certificate and "
  (let ((-aws-return-json t))
    (aws-eks "describe-cluster" "--name" name
             "--query" "cluster.{endpoint:endpoint,certificateAuthority:certificateAuthority}")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell and profile management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aws-sso-login (profile)
  (aws-sso "login" "--profile" profile))

(cl-defmacro with-aws--profile ((profile region) &rest body)
  (declare (indent defun))

  (with-gensyms (gprofile gregion)
    `(let ((,gprofile ,profile)
           (,gregion ,region))
       (with-env-vars `(("AWS_PROFILE" . ,,gprofile)
                        ("AWS_DEFAULT_REGION" . ,,gregion))
         (unless (aws-has-credentials-p)
           (aws-sso-login ,gprofile))
         ,@body))))

(defun aws--open-profile-shell (profile region)
  (with-aws--profile (profile region)
    (with-shell-buffer default-directory (format "*aws-sh-%s-%s-sh-aws*" profile region) t
      (current-buffer))))

(defun aws-profile-load-entries (config-file)
  (cl-loop for entry in (ini-parse (slurp config-file))
           for (type name) = (string-split " " (car entry))
           collect (cons name
                         (cons (cons "type" type)
                               (rest entry)))))

(defun aws-profile-list-names-from-file (config-file)
  (assoc-keys (aws-profile-load-entries config-file)))

(defun aws-profile-get-from-file (config-file profile-name)
  (assoc1 profile-name (aws-profile-load-entries config-file)))

(defun aws-profile-get-account-id-from-file (config-file profile-name)
  (assoc1 "sso_account_id" (aws-profile-get-from-file config-file profile-name)))

(cl-defun aws-configure-export-credentials (&key profile format)
  (apply #'aws-configure "export-credentials"
         `(,@(when profile
               (list "--profile" profile))
           ,@(when format
               (list "--format" format)))))

(defun aws-configure-write-credentials (profile)
  (let ((credential-file (expand-file-name "~/.aws/credentials")))
    (ini-write (list
                (cons "default"
                      (assoc1-to-assoc '(("aws_access_key_id" AccessKeyId)
                                         ("aws_secret_access_key" SecretAccessKey)
                                         ("aws_session_token" SessionToken)
                                         ("aws_credential_expiration" Expiration))
                                       (assoc1 :json (aws-configure-export-credentials :profile profile)))))
               credential-file)
    (chmod credential-file #o600 t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Route53
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aws-route53-list-hosted-zones ()
  (assoc1 '(:json HostedZones) (aws-route53 "list-hosted-zones")))

;; aws-route53-list-hosted-zones-cached
(memoize-fn aws-route53-list-hosted-zones 600)

(defun aws-route53-list-resource-record-sets (hosted-zone-id)
  (assoc1 '(:json ResourceRecordSets) (aws-route53 "list-resource-record-sets" "--hosted-zone-id" hosted-zone-id)))

;; aws-route53-list-resource-record-sets-cached
(memoize-fn aws-route53-list-resource-record-sets 600)

(defun aws-route53-list-all-records ()
  (cl-loop for zone across (aws-route53-list-hosted-zones-cached)
           collect (cons (assoc1 'Name zone)
                         (aws-route53-list-resource-record-sets-cached (assoc1 'Id zone)))))

;; aws-route53-list-all-records-cached
(memoize-fn aws-route53-list-all-records 600)

(defun aws-route53--get-resource-record (record)
  (when (assoc-get 'ResourceRecords record)
    (assoc1-traverse '(ResourceRecords 0 Value) record)))

(defun aws-route53-list-all-records-names-only ()
  (cl-loop for zone across (aws-route53-list-hosted-zones-cached)
           collect (cons (assoc1 'Name zone)
                         (cl-loop for record across (aws-route53-list-resource-record-sets-cached (assoc1 'Id zone))
                                  collect (list (assoc1 'Name record)
                                                (assoc1 'Type record)
                                                (aws-route53--get-resource-record record))))))

;;
;; This is doable, but complex
;;
;; (defconst *reverse-dns-table* (ht))

;; (defun aws-route53-list-ip-addresses ()

;;   )

;; (defun aws-route53-build-reverse-cache ())

;; (defun aws-route53-reverse-dns (ip))


;; describe-container-instances can get you the ami id.

(provide 'aws)


;;
;; Dead sql caching code.
;;
;; cmd as of now:
;; cd ~/emacs-init/mine/
;; mkdir -p ~/.emacs-data/
;; sqlite3 ~/.emacs-data/aws-task-cluster.sqlite3
;; .read sqlite-setup.sql
;; (defconst aws--emacs-data-dir "~/.emacs-data/")
;; (ensure-makedirs aws--emacs-data-dir)
;; (defconst aws--cluster-map-db (emacsql-sqlite (path-join aws--emacs-data-dir "aws-task-cluster.sqlite3")))

;; (defun aws--db (&rest cmd)
;;   (apply #'emacsql aws--cluster-map-db cmd))


;; ;;
;; ;; so in my current code,
;; ;; i can query a cluster for a service name.
;; ;;
;; ;; can I do a better check for validity?
;; ;;
;; ;; Table:
;; ;;
;; ;;  Metadata:
;; ;;     cluster_timeout
;; ;;     task_timeout
;; ;;
;; ;;  Clusters:
;; ;;     id
;; ;;     cluster-name
;; ;;     insert-time
;; ;;
;; ;;  Tasks:
;; ;;    id
;; ;;    task-name
;; ;;    cluster-id (foreign-key: cluster id)
;; ;;    insert-time
;; ;;
;; ;;  Services:
;; ;;    id
;; ;;    service-name
;; ;;
;; ;; SELECT id from tasks
;; ;;   where
;; ;;

;; ;; (defun setup-cluster-map-cache ()
;; ;;   (emacsql aws--cluster-map-db [:create-table :if :not :exists clusters
;; ;;                                   ([(id integer :primary-key)
;; ;;                                     (name text unique)
;; ;;                                     (insert-time TEXT)])])
;; ;;   )

;; (emacsql-fix-vector-indentation)

;; ;; I may need to make this more generic in the future.
;; (defmacro value-or-error (value &optional error)
;;   (with-gensyms (result)
;;      `(let ((,result ,value))
;;         (if ,result
;;             ,result
;;           (error (or ,error "Failed value check."))))))

;; (defmacro with-transaction (&rest body)
;;   ;; it might be better to do this with an exception handler
;;   ;;
;;   (with-gensyms (err)
;;       `(condition-case ,err
;;            (progn
;;              (aws--db [:begin :transaction])
;;              ,@body
;;              (aws--db [:commit :transaction]))
;;          (error (progn
;;                   (message "do rollback, err: %s" ,err)
;;                   (with-demoted-errors
;;                       (aws--db [:rollback :transaction]))
;;                   (signal (car ,err) (cdr ,err)))))))

;; (defun aws--save-cluster (cluster)
;;   (aws--db [:insert :into clusters [cluster-name environment]
;;             :values $v1] (vector cluster (current-tcc-env))))

;; (defun aws--save-task (task cluster)
;;   ;; Is it better to try to do this all in one sql statement?
;;   ;; see:
;;   ;; https://dba.stackexchange.com/questions/46410/how-do-i-insert-a-row-which-contains-a-foreign-key
;;   ;;
;;   (with-transaction
;;    (aws--db [:insert :into tasks [task-name cluster-id]
;;              :values $v1]
;;             (vector task (value-or-error
;;                           (caar (aws--db [:select id :from clusters
;;                                           :where (= cluster_name $s1)] cluster)))))))

;; (defun aws--save-instance (instance ip cluster)
;;   (with-transaction
;;    (aws--db [:insert :into instances [instance-name ip cluster-id]
;;              :values $v1]
;;             (vector instance ip
;;                     (value-or-error
;;                           (caar (aws--db [:select id :from clusters
;;                                           :where (= cluster_name $s1)] cluster)))))))

;; (defun aws--save-service (service cluster)
;;   (with-transaction
;;    (aws--db [:insert :into instances [service-name cluster-id]
;;              :values $v1]
;;              (vector instance
;;                     (value-or-error
;;                      (caar (aws--db [:select id :from clusters
;;                                      :where (= cluster_name $s1)] cluster)))))))

;; how should queries work
;;
;; query
;;   :service name
;;   :
;;
;; (defun aws--query-cm ()
;;   )

;; (defun aws-ecs-list-clusters-cached ()
;;   ;;
;;   ;; Check the db.
;;   ;;
;; )

;; (defun save-clusters (service-name)
;;   (mapc #'aws--save-cluster (aws-ecs-list-clusters)))
