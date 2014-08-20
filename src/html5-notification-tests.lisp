(in-package :html5-notification)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass test-source (source)
    ((messages :type list
               :initform nil
               :accessor messages)))
  ) ; EVAL-WHEN

(defmethod print-object ((obj test-source) out)
  (print-unreadable-object (obj out :type t)
    (format out "~s" (messages obj))))

(defmethod find-current-id ((source test-source))
  (length (messages source)))

(defmethod find-updated-objects ((source source) from-id)
  (subseq (messages source) (min from-id (length (messages source)))))

(defun add-message (source msg)
  (with-locked-instance (source)
    (setf (messages source) (append (messages source) (list msg)))
    (notify source)))

(defvar *src* (make-instance 'test-source))

#+sbcl
(defun test-wait ()
  (let ((out *debug-io*))
    (sb-thread:make-thread #'(lambda ()
                               (let ((sub (make-instance 'subscription)))
                                 (add-source sub *src*)
                                 (let ((result (wait-for-updates sub)))
                                   (format out "got result: ~s~%" result)))))))
