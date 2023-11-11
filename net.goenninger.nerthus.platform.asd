;;;; Skadi.asd
;;
;;;; Copyright (c) 2023 Goenninger B&T, Germany <support@goenninger.net>

(asdf:defsystem #:net.goenninger.nerthus.platform
  :description "Nerthus Platform - The Platform Module of the Nerthus Application Base Framework"
  :author "Goenninger B&T, Germany <support@goenninger.net>"
  :license  ""
  :version "0.0.1"
  :serial t
  :depends-on (:net.goenninger.nerthus.platform
               )
  :components ((:module nerthus.platform
                :pathname "src/platform/"
                :components
                ((:file "package")
                ))))

(defmethod perform ((o asdf:load-op) (c (eql (asdf:find-system :net.goenninger.nerthus.platform))))
  (pushnew :net.goenninger.nerthus.platform *features*))
