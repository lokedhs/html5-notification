(defpackage :html5-notification
  (:use :cl)
  (:export #:source
           #:find-updated-objects
           #:find-current-id
           #:notify
           #:source-name
           #:notification-updater
           #:simple-notify
           #:simple-source
           #:simple-named-source
           #:simple-source-initial-updated-objects
           #:named-source-mixin))

(in-package :html5-notification)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *compile-decl* '(optimize (speed 0) (safety 3) (debug 3))))
