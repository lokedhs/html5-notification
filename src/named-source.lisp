(in-package :html5-notification)

(declaim #.*compile-decl*)

(defclass named-source-mixin ()
  ((name         :type string
                 :initarg :name
                 :initform (error "~s required when using ~s" :name 'named-source-mixin)
                 :reader named-source-name)))

(defmethod source-name ((source named-source-mixin))
  (named-source-name source))
