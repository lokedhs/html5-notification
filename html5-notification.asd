(defpackage :html5-notification-system
  (:use :cl :asdf))

(in-package :html5-notification-system)

(defsystem html5-notification
  :name "html5-notification"
  :author "Elias Martenson <lokedhs@gmail.com>"
  :license "BSD"
  :description "Framework for providing EventSource-based notifications to web applications"
  :depends-on (:bordeaux-threads
               :hunchentoot
               :split-sequence
               :st-json
               :flexi-streams
               :alexandria)
  :components ((:module :src
                        :serial t
                        :components ((:file "notification-package")
                                     (:file "html5-notification")
                                     (:file "named-source")
                                     (:file "simple-source")
                                     (:file "indexed-source")
                                     (:file "html5-notification-tests")))))
