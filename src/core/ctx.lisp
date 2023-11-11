;;;; logging.lisp
;;
;;;; Copyright (c) 2023 Goenninger B&T, Germany <support@goenninger.net>

(in-package #:net.goenninger.nerthus.core)

;;; CTX CONFIG

(defparameter *ctx-store-file-pathname* (cl-fad:pathname-as-file "/tmp/nerbus-ctx.data"))
(defparameter *ctx-store-before-clear* nil)
(defparameter *ctx-store-on-set* nil)

;;; CTX

(defparameter *ctx* (make-hash-table))

;;; CTX IMPLEMENTATION

(defun config-ctx (&key (file-pathname nil file-pathname-supplied-p)
                     (store-before-clear nil store-before-clear-supplied-p)
                     (store-on-set nil store-on-set-supplied-p))
  (when file-pathname-supplied-p
    (setq *ctx-store-file-pathname* (cl-fad:pathname-as-file file-pathname)))
  (when store-before-clear-supplied-p
    (setq *ctx-store-before-clear* store-before-clear))
  (when store-on-set-supplied-p
    (setq *ctx-store-on-set* store-on-set))
  (values *ctx-store-file-pathname*
          *ctx-store-before-clear*
          *ctx-store-on-set*))

(defun ctx-set (key value &key (ctx *ctx*)
                            (file-pathname *ctx-store-file-pathname*)
                            (store-on-set *ctx-store-on-set*))
  (check-type ctx hash-table)
  (prog1
      (setf (gethash key ctx) value)
    (when store-on-set
      (ctx-store ctx file-pathname))))

(defun ctx-get (key &key (ctx *ctx*))
  (check-type ctx hash-table)
  (gethash key ctx))

(defun ctx-clear (&key (ctx *ctx*)
                    (file-pathname *ctx-store-file-pathname*)
                    (store-before-clear *ctx-store-before-clear*))
  (check-type ctx hash-table)
  (check-type file-pathname pathname)
  (when store-before-clear
    (ctx-store :ctx ctx :file-pathname file-pathname))
  (clrhash ctx))

(defun ctx-store (&key (ctx *ctx*)
                    (file-pathname *ctx-store-file-pathname*))
  (check-type ctx hash-table)
  (check-type file-pathname pathname)
  (cl-store:store ctx file-pathname)
  ctx)

(defun ctx-restore (&key (restore-*ctx* t)
                    (file-pathname *ctx-store-file-pathname*))
  (check-type file-pathname pathname)
  (let ((ctx (cl-store:restore file-pathname)))
    (when restore-*ctx*
      (setf *ctx* ctx))
    ctx))

(defun copy-ctx (ctx)
  (alexandria:copy-hash-table ctx))

(defmacro with-ctx (ctx &body body)
  `(let ((*ctx* (copy-ctx ,ctx)))
     ,@body))
