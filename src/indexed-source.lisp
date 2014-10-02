(in-package :html5-notification)

(declaim #.*compile-decl*)

(defclass indexed-source (source)
  ((index :type integer
          :initform 0
          :accessor indexed-source-index
          :documentation "The current index id")
   (state :type t
          :initarg :state
          :initform nil
          :accessor indexed-source-state
          :documentation "The current state of the source"))
  (:documentation "A source that always sends full updates after a change"))

(defmethod print-object ((obj indexed-source) out)
  (print-unreadable-object (obj out :type t)
    (format out "NAME ~s INDEX ~a" (source-name obj) (indexed-source-index obj))))

(defgeneric indexed-source-notify (source object))

(defmethod indexed-source-notify ((source indexed-source) object)
  (with-locked-instance (source)
    (incf (indexed-source-index source))
    (setf (indexed-source-state source) object))
  (notify source))

(defmethod find-updated-objects ((source indexed-source) from-id)
  (with-locked-instance (source)
    (if (= from-id (indexed-source-index source))
        nil
        (indexed-source-state source))))

(defclass indexed-source-named (indexed-source named-source-mixin)
  ()
  (:documentation "A version of INDEXED-SOURCE that has a name field."))
