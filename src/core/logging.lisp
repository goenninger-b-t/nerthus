;;;; logging.lisp
;;
;;;; Copyright (c) 2023 Goenninger B&T, Germany <support@goenninger.net>

(in-package #:net.goenninger.nerthus.core)

;;; LOGGING CONFIG

(eval-when (:compile-toplevel :execute :load-toplevel)
  (require :aserve))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (setf v:*levels* nil))

(eval-when (:load-toplevel :execute)
  (v:define-level 0 :debug)
  (v:define-level 1 :info)
  (v:define-level 2 :notice)
  (v:define-level 3 :warn)
  (v:define-level 4 :err)
  (v:define-level 5 :crit)
  (v:define-level 6 :alert)
  (v:define-level 7 :emerg))

;;; LOGGING IMPLEMENTATION

(defun init-logging (&key (ctx *ctx*))
  (setq v:*timestamp-format* local-time:+iso-8601-format+)
  ;; TODO: Make REPL logging level configurable via ctx / configuration file
  (setf (v:repl-level) :debug)
  (values))

(defun shutdown-logging (&key (ctx *ctx*))
  (declare (ignore ctx))
  (setf (v:repl-level) :notice)
  (values))

(defclass log-message ()
  ((message-code      :reader message-code     :initarg :message-code     :type integer)
   (reason-code       :reader reason-code      :initarg :reason-code      :type integer)
   (message-id        :reader message-id       :initarg :message-id       :type uuid:uuid)
   (application-name  :reader application-name :initarg :application-name :type simple-string)
   (domain-id         :reader domain-id        :initarg :domain-id        :type integer)
   (partition         :reader partition        :initarg :partition        :type simple-string)
   (location-info     :reader location-info    :initarg :location-info    :type simple-string)
   (severity-level    :reader severity-level   :initarg :severity-level    :type integer)
   (timestamp         :reader timestamp        :initarg :timestamp        :type simple-string)
   (message-format    :reader message-format   :initarg :message-format   :type simple-string)
   (message-args      :reader message-args     :initarg :message-args))
   (call-stack-info   :reader call-stack-info  :initarg :call-stack-info)))

;;; LOGGING TO SLACK

(declaim (inline slack-logging-enabled-p))
(defun slack-logging-enabled-p (&key (ctx *ctx*))
  (declare (type hash-table ctx))
  (declare (optimize (speed 3) (space 0) (debug 0) (compilation-speed 0)))
  (ctx-get :slack-logging-enabled :ctx ctx))

(defmethod slack-log ((slack-message slack-message)(slack-connection-info slack-connection-info) )
  (send-to-slack (json-format-slack-message slack-message) slack-connection-info))
