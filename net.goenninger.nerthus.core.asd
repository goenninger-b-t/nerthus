;;;; Skadi.asd
;;
;;;; Copyright (c) 2023 Goenninger B&T, Germany <support@goenninger.net>

(asdf:defsystem #:net.goenninger.nerthus.core
  :description "Nerthus Core - The Core Module of the Nerthus Application Base Framework"
  :author "Goenninger B&T, Germany <support@goenninger.net>"
  :license  ""
  :version "0.0.1"
  :serial t
  :depends-on (:closer-mop           ;; MOP stuff
               :trivial-features     ;; Endianness handling
               :alexandria           ;; For a lot of different things
               :cl-store             ;; Persisting objects, e.g. CTX
               :verbose              ;; Logging
               :yason                ;; JSON input putput
               :json-mop             ;; CLOS to JSON bridge
               :uuid                 ;; Object IDs are UUIDs
               :local-time           ;; Timestamps
               )
  :components ((:module nerthus.core
                :pathname "src/core/"
                :components
                ((:file "package")
                 (:file "process-pool")
                 (:file "ctx")
                 (:file "logging")
                 (:file "slack-message")
                ))))

(defmethod perform ((o asdf:load-op) (c (eql (asdf:find-system :net.goenninger.nerthus.core))))
  (pushnew :net.goenninger.nerthus.core *features*))
