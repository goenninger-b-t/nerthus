;;;; logging.lisp
;;
;;;; Copyright (c) 2023 Goenninger B&T, Germany <support@goenninger.net>

(in-package #:net.goenninger.nerthus.core)

;;; LOGGING CONFIG

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

(defparameter *message-uuid-null-message* (uuid:make-null-uuid))

(defparameter *log-message-default-application-name* "UNKNOWN")
(defparameter *log-message-default-component-uuid* (uuid:make-null-uuid))
(defparameter *log-message-default-domain-id* -1)
(defparameter *log-message-default-partition* "/")
(defparameter *log-message-default-location-info* "UNKNOWN")


(defclass v::nerthus-message (v:message)
  ((message-code      :reader message-code     :initarg :message-code     :type integer)
   (reason-code       :reader reason-code      :initarg :reason-code      :type integer)
   (message-uuid      :reader message-uuid     :initarg :message-uuid     :type uuid:uuid)
   (application-name  :reader application-name :initarg :application-name :type simple-string)
   (component-uuid    :reader component-uuid   :initarg :component-uuid   :type uuid:uuid)
   (domain-id         :reader domain-id        :initarg :domain-id        :type integer)
   (partition         :reader partition        :initarg :partition        :type simple-string)
   (location-info     :reader location-info    :initarg :location-info    :type simple-string))
  (:default-initargs
   :message-code 0
   :reason-code 0
   :message-uuid *message-uuid-null-message*
   :application-name *log-message-default-application-name*
   :component-uuid *log-message-default-component-uuid*
   :domain-id *log-message-default-domain-id*
   :partition *log-message-default-partition*
   :location-info *log-message-default-location-info*
   ))


#|
                                        ; ;
| APPLICATION: ADTLATUS | DOMAIN: 1 /   ; ;
| COMPONENT-ID: ADT-200A 11004079 DG1SBG ; ;
| COMP. LOCATION: Earth                 ; ;
| MSG-ID: 43001f35-7880-4d94-b7eb-66db69492d6 ; ;
| MSG-CODE: 10100021                    ; ;
| REASON-CODE: 10131097                 ; ;
| MSG: Bumble bees                      ; ;
| MSG CTX: stack...                     ; ;
                                        ; ;
|#


(defmethod v:format-message ((stream stream) (message v::nerthus-message))
  (break)
  (format stream "~a [~5,a] ~{<~a>~} | APPLICATION: ~a | MSG ~a"
          (local-time:format-timestring nil (v:timestamp message) :format v:*timestamp-format*)
          (v:level message)
          (v:categories message)
          (application-name message)
          (format-message nil (v:content message))))
                                        ; ;

(defun init-logging (&key (ctx *ctx*))
  (setq v:*timestamp-format* local-time:+iso-8601-format+)
  (setq v:*default-message-class* 'v::nerthus-message)
  ;; TODO: Make REPL logging level configurable via ctx / configuration file
  (setf (v:repl-level) :debug)
  (values))

(defun shutdown-logging (&key (ctx *ctx*))
  (declare (ignore ctx))
  (setf (v:repl-level) :notice)
  (values))
