(in-package :html5-notification)

(declaim #.*compile-decl*)

(defclass simple-source (source)
  ((queue        :type list
                 :initform nil
                 :accessor simple-source-queue
                 :documentation "A list of entries. Each entry is a list of three
elements: id, date and object.")
   (max-history  :initarg :max-history
                 :initform (* 5 60)
                 :accessor simple-source-max-history)
   (current-id   :type alexandria:positive-integer
                 :initform 1
                 :accessor simple-source-current-id)))

(defmethod print-object ((obj simple-source) out)
  (print-unreadable-object (obj out :type t)
    (with-slots (queue max-history current-id) obj
      (format out "NAME ~s QUEUE-LENGTH ~s MAX-HISTORY ~s CURRENT-ID ~s"
              (source-name obj) (length queue) max-history current-id))))

(defclass simple-named-source (simple-source named-source-mixin)
  ()
  (:documentation "A version of SIMPLE-SOURCE that has a name field."))

(defgeneric simple-notify (source object))

(defmethod simple-notify ((source simple-source) object)
  (with-locked-instance (source)
    (with-slots (queue max-history current-id) source
      ;; Clear expired entries
      (loop
         with cutoff = (- (get-universal-time) max-history)
         with prev = nil
         for v on queue
         while (>= (cadar v) cutoff)
         do (setq prev v)
         finally (when prev (rplacd prev nil)))
    ;; Push the current element to the top of the list
    (push (list (incf current-id)
                (get-universal-time)
                object)
          queue)))
  ;; Finally notify the the source that there are available updates
  (notify source))

(defgeneric simple-source-initial-updated-objects (source)
  (:documentation "Return the initial list of updated objects."))

(defmethod simple-source-initial-updated-objects ((source t))
  "Default implementation that returns NIL"
  nil)

(defmethod find-updated-objects ((source simple-source) from-id)
  (if (null from-id)
      ;; with an empty FROM-ID, request the full object list, defaults to NIL
      (simple-source-initial-updated-objects source)
      ;; ELSE form: find updated objects from the queue
      (with-locked-instance (source)
        (with-slots (queue) source
          ;; The resulting list needs to be reversed as we want to return the oldest objects first
          (reverse (loop
                      for (id date object) in queue
                      while (> id from-id)
                      collect object))))))

(defmethod find-current-id ((source simple-source))
  "The current ID of a SIMPLE-SOURCE is simply the id of the first element in the queue."
  (or (caar (simple-source-queue source)) 0))
