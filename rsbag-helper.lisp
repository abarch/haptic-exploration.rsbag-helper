(in-package #:rsbag-helper)

(defvar *rst-path* NIL)
(defvar *sequence-number* 0)

(defun maybe-rst-path (domain)
  (merge-pathnames
   (make-pathname :directory (list :relative domain))
   (merge-pathnames  
    #P"rst-proto/proto/"
    (or (uiop:parse-native-namestring (uiop:getenv "RST"))
	*default-pathname-defaults*))))

(defun load-proto-file (pathname)
  (let ((pbf:*proto-load-path* (list* (maybe-rst-path "stable" ) (maybe-rst-path "sandbox") pbf:*proto-load-path*)))
    (when *rst-path* (push *rst-path* pbf:*proto-load-path*))
    (rsb.common:load-idl pathname :auto :purpose '(:packed-size :serializer :deserializer))))

(defun make-precise-timestamp (universal)
  (multiple-value-bind (secs fractional-secs) (floor universal 1)
    (local-time:universal-to-timestamp secs :nsec (floor (* fractional-secs 1000000000)))))

(defun ensure-keyword (thing)
  (etypecase thing
    (keyword thing)
    (symbol (ensure-keyword (symbol-name thing)))
    (string (intern thing "KEYWORD"))))

(defclass bag-channel ()
  ((bag :initarg :bag :accessor bag)
   (channel :initarg :channel :accessor channel)
   (id :initarg :id :accessor id)
   (scope :initarg :scope :accessor scope)
   (type :initarg :type :accessor channel-type))
  (:default-initargs
   :bag (error "BAG required.")
   :id (uuid:make-v1-uuid)
   :scope (error "SCOPE required.")
   :type (error "TYPE required.")))

(defmethod initialize-instance :after ((bag-channel bag-channel) &key)
  (setf (channel-type bag-channel)
        (ensure-keyword (channel-type bag-channel)))
  (unless (slot-boundp bag-channel 'channel)
    (with-slots (bag channel id scope type) bag-channel
      (setf channel
            (or (rsbag:bag-channel bag (format NIL "~a:~a" scope type) :if-does-not-exist NIL)
                (setf (rsbag:bag-channel bag (format NIL "~a:~a" scope type))
                      `(:type (:rsb-event-0.8 ,type)
                        :source-name ,(princ-to-string id)
                        :source-config ,(format nil "rsb:/#~A" id))))))))

(defmethod print-object ((channel bag-channel) stream)
  (print-unreadable-object (channel stream :type T)
    (format stream "~a ~a:~a"
            (id channel) (scope channel) (channel-type channel))))

(defgeneric to-vicon-object (object)
  (:method (object) object))

(defun make-entry (bag-channel payload timestamp &key (sequence-number (incf *sequence-number*))
                                                        (create timestamp)
                                                        (send timestamp)
                                                        (receive timestamp)
                                                        (deliver timestamp))
  (let ((vicon (to-vicon-object payload)))
    (with-slots (channel scope id) bag-channel
      (setf (rsbag:entry channel timestamp)
            (let ((event (rsb:make-event scope vicon)))
              (setf (rsb:event-sequence-number event) sequence-number
                    (rsb:event-origin event) id
                    (rsb:timestamp event :create) create
                    (rsb:timestamp event :send) send
                    (rsb:timestamp event :receive) receive
                    (rsb:timestamp event :deliver) deliver)
              event)))))

(defun make-default-transform ()
  `(rsbag:&from-source
    :converter ,(cdr (assoc 'nibbles:octet-vector
                            (rsb:default-converters)))))

(defmacro with-default-bag ((bag pathname &key (direction :io)
                                               (if-exists :supersede)
                                               (transform '(make-default-transform))) &body body)
  `(rsbag:with-bag (,bag ,pathname
                         :direction ,direction
                         :if-exists ,if-exists
                         :transform ,transform)
     ,@body))

(defmacro with-bag-channel ((channel bag scope type) &body body)
  `(let ((,channel (make-instance 'bag-channel :bag ,bag :scope ,scope :type ,type))
         (*sequence-number* 0))
     ,@body))

(defun process-file-descriptor (file-descriptor stream-processor &key file)
  (etypecase file-descriptor
    (list
     (dolist (desc file-descriptor)
       (process-file-descriptor desc stream-processor)))
    (string
     (process-file-descriptor (uiop:parse-native-namestring file-descriptor) stream-processor))
    (pathname
     (cond
       ((wild-pathname-p file-descriptor)
        (mapc (alexandria:rcurry #'process-file-descriptor stream-processor)
              (directory file-descriptor)))
       ((string-equal (pathname-type file-descriptor) "zip")
        (zip:with-zipfile (file file-descriptor)
          (process-file-descriptor file stream-processor)))
       (T
        (alexandria:with-input-from-file (stream file-descriptor)
          (process-file-descriptor stream stream-processor :file (pathname stream))))))
    (zip:zipfile
     (zip:do-zipfile-entries (name entry file-descriptor)
       (with-simple-restart (skip "Skip zip-file entry ~A" name)
         (process-file-descriptor
          (flexi-streams:make-flexi-stream
           (flexi-streams:make-in-memory-input-stream
            (zip:zipfile-entry-contents entry)))
          stream-processor :file (uiop:parse-native-namestring name)))))
    (stream
     (funcall stream-processor file-descriptor file))))

(defmacro with-file-descriptor ((stream file file-descriptor) &body body)
  `(process-file-descriptor ,file-descriptor (lambda (,stream ,file) ,@body)))
