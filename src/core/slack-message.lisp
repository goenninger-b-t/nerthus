;;;; logging.lisp
;;
;;;; Copyright (c) 2023 Goenninger B&T, Germany <support@goenninger.net>

(in-package #:net.goenninger.nerthus.core)

(eval-when (:compile-toplevel :execute :load-toplevel)
  (require :aserve))

;;; SLACK MESSAGE CONFIG


;;; LOGGING TO SLACK

(declaim (inline slack-logging-enabled-p))
(defun slack-logging-enabled-p (&key (ctx *ctx*))
  (declare (type hash-table ctx))
  (declare (optimize (speed 3) (space 0) (debug 0) (compilation-speed 0)))
  (ctx-get :slack-logging-enabled :ctx ctx))

;;; https://slack.com/oauth/v2/authorize?scope=incoming-webhook,commands&client_id=3336676.569200954261

(defparameter *slack-request-timeout* 2) ;; in seconds
(defparameter *slack-workspace-name* "nerbus")
(defparameter *slack-webhook-url* "https://hooks.slack.com/services/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")
(defparameter *slack-oauth-token* "xoxb-414361922979-6140670431206-dt8xCjPu0IA3vTgbAXlArgsu")

(defclass slack-connection-info ()
  ((workspace-name     :reader workspace-name     :initarg :workspace-name)
   (webhook-url        :reader webhook-url        :initarg :webhook-url)
   (oauth-bearer-token :reader oauth-bearer-token :initarg :oauth-bearer-token))
  (:default-initargs
   :workspace-name *slack-workspace-name*
   :webhook-url *slack-webhook-url*
   :oauth-bearer-token *slack-oauth-token*))

(defparameter *slack-attachment-color-emerg*   "b60017") ;; PANTONE 20-0069 TPM
(defparameter *slack-attachment-color-alert*   "b60017") ;; PANTONE 20-0069 TPM
(defparameter *slack-attachment-color-crit*    "b60017") ;; PANTONE 20-0069 TPM
(defparameter *slack-attachment-color-err*     "ec4600") ;; PANTONE 20-0059 TPM
(defparameter *slack-attachment-color-warn*    "ddc738") ;; PANTONE 20-0045 TPM
(defparameter *slack-attachment-color-notice*  "ddc738") ;; PANTONE 20-0045 TPM
(defparameter *slack-attachment-color-info*    "ada59f") ;; PANTONE 20-0089 TPM
(defparameter *slack-attachment-color-debug*   "e6e9e4") ;; PANTONE 20-0081 TPM

(defparameter *slack-attachment-color-default* "e6e9e4") ;; PANTONE 20-0081 TPM

(defparameter *severity-level-to-slack-attachment-color*
  '((0 . #.*slack-attachment-color-debug*)
    (1 . #.*slack-attachment-color-info*)
    (2 . #.*slack-attachment-color-notice*)
    (3 . #.*slack-attachment-color-warn*)
    (4 . #.*slack-attachment-color-err*)
    (5 . #.*slack-attachment-color-crit*)
    (6 . #.*slack-attachment-color-alert*)
    (7 . #.*slack-attachment-color-emerg*)))

(declaim (inline log-severity-level->slack-attachment-colo))
(defun log-severity-level->slack-attachment-color (log-severity-level)
  (declare (type integer log-severity-level))
  (declare (optimize (speed 3) (size 0) (debug 0) (compilation-speed 0)))
  (or (cdr (find log-severity-level *severity-level-to-slack-attachment-color* :key #'car))
      *slack-attachment-color-default*))

(defparameter *slack-application-name* "Nebrus")
(defparameter *slack-domain-name* "/gbt/nebrus/log")
(defparameter *slack-domain-id* 0)
(defparameter *slack-location-info* "Earth")
(defparameter *slack-severity-level* 1)

(defparameter *slack-message-seq-nr-base*         0)
(defparameter *slack-message-seq-nr-start-offset* 0)

(defun default-slack-next-message-seq-nr (last-slack-message-seq-nr)
   (incf last-slack-message-seq-nr))

(defparameter *slack-message-seq-nr-fn* #'default-slack-next-message-seq-nr*)

(let ((last-slack-message-seq-nr (+ *slack-message-seq-nr-base* *slack-message-seq-nr-start-offset*)))
  (defun slack-next-message-seq-nr ()
    (setq last-slack-message-seq-nr (apply *slack-message-seq-nr-fn*))


(defclass slack-message ()
  ((message-id        :reader   message-id       :initarg :message-id)
   (application-name  :reader   application-name :initarg :application-name :type simple-string)
   (domain-name       :reader   domain-name      :initarg :domain-name      :type simple-string)
   (domain-id         :reader   domain-id        :initarg :domain-id        :type integer)
   (location-info     :reader   location-info    :initarg :location-info    :type simple-string)
   (severity-level    :reader   severity-level   :initarg :severity-level    :type integer)
   (timestamp         :reader   timestamp        :initarg :timestamp        :type simple-string)
   (message-format    :reader   message-format   :initarg :message-format   :type simple-string)
   (message-args      :accessor message-args     :initarg :message-args))
  (:default-initargs
   :application-name *slack-application-name*
   :domain-name *slack-domain-name*
   :domain-id *slack-domain-id*
   :location-info *slack-location-info*
   :severity-level *slack-severity-level*
   :timestamp (local-time:now)
   :message-format nil
   :message-args nil))

(defun make-slack-message (class &rest args)
  (apply #'make-instance class args))

(defmethod yason:encode-slots progn ((slack-message slack-message))
  (with-slots (application-name domain-name location-info severity-level timestamp message-format message-args) slack-message
    (apply message-format
           application-name
           domain-name
           location-info
           severity-level
           timestamp
           message-args)))

(defparameter *slack-format-message-fn*
  (lambda (slack-message)
    (check-type slack-message slack-message)
    (yason:encode-object slack-message)))

(defmethod json-format-slack-message ((slack-message slack-message))
  (apply *slack-format-message-fn* slack-message))

;;; ---
;;; Content-type: application/json' --data '{"text":"Hello, :RED_CIRCLE:"}'
;;;  https://hooks.slack.com/services/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
;;; ---

(defmethod send-to-slack ((slack-connection-info slack-connection-info) json-msg-str)
  (with-slots (workspace-name webhook-url oauth-bearer-token) slack-connection-info
    (net.aserve.client:do-http-request
      )))

(defmethod slack-log ((slack-message slack-message)(slack-connection-info slack-connection-info) )
  (send-to-slack (json-format-slack-message slack-message) slack-connection-info))
