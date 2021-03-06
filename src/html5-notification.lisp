(in-package :html5-notification)

(declaim #.*compile-decl*)

(alexandria:define-constant +CRLF+ (format nil "~c~c" #\Return #\Newline) :test 'equal)

(defvar *maximum-notification-wait-seconds* 30
  "The maximum time to wait for updates to any data source before sending a ping message to the browser")

;;;
;;;  LOCKABLE-INSTANCE-MIXIN
;;;

(defclass lockable-instance-mixin ()
  ((lock          :initform (bordeaux-threads:make-recursive-lock "Class instance lock")
                  :reader lockable-instance-lock)
   (cond-variable :initform (bordeaux-threads:make-condition-variable :name "Class instance condvar")
                  :reader lockable-instance-cond-variable)))

(defmacro with-locked-instance ((instance) &body body)
  `(bordeaux-threads:with-recursive-lock-held ((lockable-instance-lock ,instance))
    ,@body))

;;;
;;;  SOURCE
;;;

(defclass source (lockable-instance-mixin)
  ((listeners :type hash-table
              :initform (make-hash-table)
              :accessor source-listeners
              :documentation "Map of functions that are to be called with the new id whenever the source is updated."))
  (:documentation "Superclass for all notification sources"))

(defmethod print-object ((obj source) out)
  (print-unreadable-object (obj out :type t)
    (format out "LISTENERS ~s" (source-listeners obj))))

(defgeneric find-updated-objects (source from-id)
  (:documentation "Returns a list of elements with an id higher than FROM-ID."))

(defgeneric find-initial-objects (source num-objects from-id)
  (:documentation "Return a list of NUM-OBJECTS most recent objects."))

(defmethod find-initial-objects ((source t) num-objects from-id)
  "Default implementation that returns NIL."
  nil)

(defgeneric find-current-id (source)
  (:documentation "Returns the current id of SOURCE."))

(defgeneric source-name (source)
  (:documentation "Returns the name of the source. Used as a key in the JSON output."))

(defun add-listener (reference callback)
  (let ((source (subscription-entry-source reference))
        (from-id (subscription-entry-last-id reference)))
    (with-locked-instance (source)
      (setf (gethash reference (source-listeners source)) callback)
      (when (or (null from-id)
                (not (equal from-id (find-current-id source))))
        (funcall callback)))))

(defun remove-listener (reference)
  (let ((source (subscription-entry-source reference)))
    (with-locked-instance (source)
      (unless (remhash reference (source-listeners source))
        (log:error "Trying to remove nonexistent listener. source = ~s reference = ~s" source reference)))))

(defgeneric notify (source)
  (:documentation "Notify the listeners that SOURCE has been updated."))

(defmethod notify ((source source))
  (let ((elements (with-locked-instance (source)
                    (loop
                       for element being the hash-value of (source-listeners source)
                       collect element))))
    (log:trace "Notify start for source: ~s, num-elements: ~a" source (length elements))
    (dolist (element elements)
      (funcall element))))

;;;
;;;  SUBSCRIPTION
;;;

(defclass subscription-entry ()
  ((source                  :type source
                            :initform (error "source must be specified")
                            :initarg :source
                            :reader subscription-entry-source)
   (last-id                 :type t
                            :initform (error "~s required when creating ~s" :last-id 'subscription-entry)
                            :initarg :last-id
                            :accessor subscription-entry-last-id)
   (json-translate-function :type function
                            :initform #'identity
                            :initarg :json-translate-function
                            :reader subscription-entry-translation-function)
   (filter                  :type function
                            :initarg :filter
                            :reader subscription-entry-filter)
   (num-objects             :type (integer 0)
                            :initform 0
                            :initarg :num-objects
                            :reader subscription-entry-num-objects
                            :documentation "The number of initial objects to return when no event-id was passed in")
   (from-id                 :type (or null string)
                            :initarg :from-id
                            :initform nil
                            :reader subscription-entry-from-id
                            :documentation "An identifier indicating from where to start returning results.")))
                       
(defclass subscription (lockable-instance-mixin)
  ((entries     :type list
                :initform nil
                :accessor subscription-entries
                :documentation "List of instances of SUBSCRIPTION-ENTRY")
   (queue       :type list
                :initform nil
                :accessor subscription-queue
                :documentation "Queue which the updates are pushed to"))
  (:documentation "Class representing an active subscription to a set
of sources. The instance is valid while the client is actively
listening to updates from the source."))

(defmethod initialize-instance :after ((obj subscription) &key sources http-event)
  (let ((parsed (parse-http-event http-event)))
    (dolist (source-descriptor sources)
      (destructuring-bind (source &key filter translation-function num-objects from-id) source-descriptor
        (add-source obj source
                    :last-id (http-event-value (source-name source) parsed)
                    :translation-function translation-function
                    :filter filter
                    :num-objects (or num-objects 0)
                    :from-id from-id)))))

(defun add-source (subscription source &key last-id translation-function filter num-objects from-id)
  (check-type subscription subscription)
  (check-type source source)
  (let ((entry (make-instance 'subscription-entry
                              :source source
                              :last-id last-id
                              :json-translate-function (or translation-function #'identity)
                              :filter (or filter (constantly t))
                              :num-objects num-objects
                              :from-id from-id)))
    (with-locked-instance (subscription)
      (push entry (subscription-entries subscription)))))

(defun updated-objects-from-entry (entry)
  (check-type entry subscription-entry)
  (with-slots (source last-id json-translate-function filter) entry
    (destructuring-bind (result new-id)
        (with-locked-instance (source)
          (list (if last-id
                    (find-updated-objects source last-id)
                    (find-initial-objects source
                                          (subscription-entry-num-objects entry)
                                          (subscription-entry-from-id entry)))
                (find-current-id source)))
      ;; Make each updated object into the following format:
      ;; {
      ;;   type: "type-name",
      ;;   element: { ... }
      ;; }
      (flet ((make-element (res)
               (when (funcall filter res)
                 (st-json:jso "type" (source-name source)
                              "element" (funcall json-translate-function res)))))
        (let ((prefixed (etypecase result
                          (list (loop for e in result for v = (make-element e) when v collect v ))
                          (array (loop for e across result for v = (make-element e) when v collect v )))))
          ;; Append the updated objects to the queue
          (values prefixed new-id))))))

(defun wait-for-updates (subscription before-wait-callback expire)
  "Wait until any of the sources in SUBSCRIPTION has been updated and return the updates.
If no updates has happened until *MAXIMUM-NOTIFICATION-WAIT-SECONDS* seconds
has elapsed, return NIL."
  (check-type subscription subscription)
  (flet ((push-update (e)
           (with-locked-instance (subscription)
             (pushnew e (subscription-queue subscription))
             (bordeaux-threads:condition-notify (lockable-instance-cond-variable subscription)))))

    (block updater
      (unwind-protect
           (progn
             (dolist (e (subscription-entries subscription))
               (let ((entry e))         ; Ensure a new binding
                 (add-listener entry #'(lambda () (push-update entry)))))
             (let* ((now (get-universal-time))
                    (timeout (min (+ now *maximum-notification-wait-seconds*)
                                  expire)))
               (loop
                  for remaining = (- timeout (get-universal-time))
                  while (plusp remaining)
                  do (with-locked-instance (subscription)
                       (when (subscription-queue subscription)
                         (return-from updater nil))
                       (when before-wait-callback
                         (funcall before-wait-callback))
                       ;; The below code uses a platform-specific version of condition-wait for SBCL.
                       ;; This is because of a bug in SBCL that prevents with-timeout to actually
                       ;; perform a timeout under certain circumstances.
                       ;;
                       ;; The workaround solution is the proper one to use anyway, as with-timeout
                       ;; is considered unsafe.
                       #-sbcl
                       (progn
                         (handler-case
                             (bordeaux-threads:with-timeout (remaining)
                               (bordeaux-threads:condition-wait (lockable-instance-cond-variable subscription)
                                                                (lockable-instance-lock subscription)))
                           (bordeaux-threads:timeout (v) v)))
                       #+sbcl
                       (sb-thread:condition-wait (lockable-instance-cond-variable subscription)
                                                 (lockable-instance-lock subscription)
                                                 :timeout remaining)))))
        ;; Unwind form: Make sure that all listeners are removed before exiting scope
        (dolist (e (subscription-entries subscription))
          (remove-listener e))))
    ;; At this point we may or may not have any updated entries.
    ;; However, there may still be some updates coming in since the
    ;; updates are asynchronous from the sender. Thus, we copy it
    ;; while holding the lock.
    (let ((result nil))
      (with-locked-instance (subscription)
        (dolist (e (subscription-queue subscription))
          (multiple-value-bind (prefixed new-id)
              (updated-objects-from-entry e)
            (setf (subscription-entry-last-id e) new-id)
            (setq result (append result prefixed))))
        (setf (subscription-queue subscription) nil))
      result)))

(defun encode-name (string)
  (check-type string string)
  (with-output-to-string (s)
    (loop
       for ch across string
       for code = (char-code ch)
       if (or (<= (char-code #\a) code (char-code #\z))
              (<= (char-code #\A) code (char-code #\Z))
              (<= (char-code #\0) code (char-code #\9))
              (eql ch #\@)
              (eql ch #\_)
              (eql ch #\.)
              (eql ch #\,)
              (> code 255))
       do (princ ch s)
       else
       do (format s "!~2,'0x" code))))

(defun decode-name (string)
  (check-type string string)
  (with-output-to-string (s)
    (loop
       with len = (length string)
       with i = 0
       while (< i len)
       for ch = (aref string i)
       do (incf i)
       if (eq ch #\!)
       do (progn
            (princ (code-char (parse-integer string :start i :end (+ 2 i) :radix 16)) s)
            (incf i 2))
       else do (princ ch s))))

(defun id-string-from-sub (sub)
  (format nil "~{~a~^:~}"
          (loop
             for entry in (subscription-entries sub)
             for id = (subscription-entry-last-id entry)
             when id
             collect (format nil "~a:~a"
                             (encode-name (source-name (subscription-entry-source entry)))
                             (encode-name id)))))

(defun parse-http-event (header)
  (when header
    (labels ((split-part (list)
               (unless (endp list)
                 (cons (cons (decode-name (car list)) (decode-name (cadr list)))
                       (split-part (cddr list))))))
      (split-part (split-sequence:split-sequence #\: header)))))

(defun http-event-value (key list)
  (cdr (find (string key) list :key #'car :test #'equal)))

(defun format-update-message-text (subscription prefixed)
  (with-output-to-string (out)
    (format out "id:~a~a" (id-string-from-sub subscription) +CRLF+)
    (format out "data:")
    (st-json:write-json prefixed out)
    (format out "~a~a" +CRLF+ +CRLF+)))

(defun send-ping-message (out)
  (format out ":none~a~a" +CRLF+ +CRLF+))

(defun notification-updater (sources &key before-wait-callback after-write-callback (max-connection 600))
  "Main loop that wait for updates from the given sources and sends the updated
results back to the client.

SOURCES is a list of source descriptors. Each source descriptor have
the following form:

\(source :filter filter :translation-function translation-function).

SOURCE is the data source, FILTER is a
function that is called on the data and returns true if the data
should be delivered and TRANSLATION-FUNCTION is a function that
formats the data object to JSON.

The default for FILTER is \(CONSTANTLY T) and the default for
PRINTER is #'INDENTITY.

If the BEFORE-WAIT-CALLBACK keyword argument is non-NIL it is assumed
to be a function which will be called just before the thread is blocking
while waiting for updated. This callback can be used to release resources
that are not needed while the thread is waiting.

If MAX-CONNECTION is non-nil, it indicates the max number of seconds
that the connection is allowed to be active. After this time, the
connection will be closed."
  (check-type max-connection (integer 0))
  (handler-bind (#+sbcl
                 (sb-int:simple-stream-error #'(lambda (cond)
                                                 (declare (ignore cond))
                                                 (log:warn "Got SIMPLE-STREAM-ERROR~%")
                                                 ;;(hunchentoot:abort-request-handler)
                                                 )))
    (setf (hunchentoot:header-out :cache-control) "no-cache")
    (setf (hunchentoot:content-type*) "text/event-stream")
    (let* ((out (flexi-streams:make-flexi-stream (hunchentoot:send-headers)
                                                 :external-format :utf8))
           (dont-loop (equal (hunchentoot:get-parameter "no_loops") "1"))
           (sub (make-instance 'subscription :http-event (hunchentoot:header-in* :last-event-id) :sources sources))
           (expire (+ (get-universal-time) max-connection)))
      (loop
         do (let ((result (wait-for-updates sub before-wait-callback expire)))
              (if (null result)
                  (send-ping-message out)
                  (princ (format-update-message-text sub result) out))
              (finish-output out)
              (when after-write-callback
                (funcall after-write-callback)))
         while (and (not dont-loop)
                    (< (get-universal-time) expire))))))

(defun get-single-update (event-id sources &key (max-connection 600))
  (check-type max-connection (integer 0))
  (let ((sub (make-instance 'subscription :http-event event-id :sources sources)))
    (log:trace "Waiting for updates from: ~s" sources)
    (let ((result (wait-for-updates sub nil (+ (get-universal-time) max-connection))))
      (log:trace "Got updates: ~s" result)
      (list (id-string-from-sub sub) result))))
