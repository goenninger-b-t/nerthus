;;;; Skadi.asd
;;
;;;; Copyright (c) 2023 Goenninger B&T, Germany <support@goenninger.net>

(asdf:defsystem #:net.goenninger.nerthus.eventbus
  :description "Nerthus Eventbus - The Eventbus Module of the Nerbus Application Base Framework"
  :author "Goenninger B&T, Germany <support@goenninger.net>"
  :license  ""
  :version "0.0.1"
  :serial t
  :depends-on (:net.goenninger.nerthus.core
               )
  :components ((:module nerthus.eventbus
                :pathname "src/eventbus/"
                :components
                ((:file "package")
                ))))

(defmethod perform ((o asdf:load-op) (c (eql (asdf:find-system :net.goenninger.nerthus.eventbus))))
  (pushnew :net.goenninger.nerthus.eventbus *features*))
