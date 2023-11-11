;;;; logging.lisp
;;
;;;; Copyright (c) 2023 Goenninger B&T, Germany <support@goenninger.net>

(in-package #:net.goenninger.nerthus.core)

;;; LOGGING TO SLACK

(declaim (inline slack-logging-enabled-p))
(defun slack-logging-enabled-p (&key (ctx *ctx*))
  (declare (type hash-table ctx))
  (declare (optimize (speed 3) (space 0) (debug 0) (compilation-speed 0)))
  (ctx-get :slack-logging-enabled :ctx ctx))

(defmethod slack-log ((slack-message slack-message)(slack-connection-info slack-connection-info) )
  (send-to-slack (json-format-slack-message slack-message) slack-connection-info))

