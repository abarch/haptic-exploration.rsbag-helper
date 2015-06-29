(in-package #:rsbag-helper)

(defmacro setdocs (&body pairs)
  `(progn
     ,@(loop for (var doc) in pairs
             collect (destructuring-bind (var &optional (type 'function))
                         (if (listp var) var (list var))
                       `(setf (documentation ',var ',type) ,doc)))))

(setdocs
 ((*rst-path* variable)
  "Pathname to search for protobuf definitions.")

 ((*sequence-number* variable)
  "The counter that determines what sequence number to use for an event.")

 (maybe-rst-path
  "Try to automatically determine a path for RST protobuf definitions.
This merges \"rst-proto/proto/stable/\" with either the \"RST\" env-var
or *DEFAULT-PATHNAME-DEFAULTS*.")

 (load-proto-file
  "Attempt to load a protobuf file on PATHNAME.")

 (make-precise-timestamp
  "Create a LOCAL-TIME:TIMESTAMP, but precise to a fractional UNIVERSAL-time.")

 (ensure-keyword
  "Make sure that THING is either a KEYWORD, or turned into one.")

 ((bag-channel type)
  "A wrapper class to represent a channel on a BAG.")

 (bag
  "Return the bag the channel is a member of.")

 (channel
  "Return the channel that this object wraps.")

 (id
  "Return the channel's id.")

 (scope
  "Return the channel's scope.")

 (type
  "Return the channel's type.")

 (to-vicon-object
  "A generic function that should turn OBJECT into an RST type that can be dumped.")

 (make-entry
  "Create a new entry in the BAG-CHANNEL, with PAYLOAD as the data.

TIMESTAMP will be used as the timestamp for CREATE, SEND, RECEIVE, and DELIVER
unless specifically overridden in the according keyword arguments.")

 (make-default-transform
  "Create a standard transform source to convert octet-vectors with RSB:DEFAULT-CONVERTERS.")

 (with-default-bag
   "Minor macro around RSBAG:WITH-BAG to make the common path easier.")

 (with-bag-channel
   "Minor macro to create a BAG-CHANNEL and bind it.")

 (process-file-descriptor
  "Process FILE-DESCRIPTOR as appropriate and call STREAM-PROCESSOR with the resulting stream and file.

FILE-DESCRIPTOR can be one of the following:
  LIST         --- Call PROCESS-FILE-DESCRIPTOR with each element in the list.
  STRING       --- Call PROCESS-FILE-DESCRIPTOR with the result of UIOP:PARSE-NATIVE-NAMESTRING.
  PATHNAME     --- Wild pathname: Call PROCESS-FILE-DESCRIPTOR for each pathname returned by DIRECTORY
                   Pathname-type of \"zip\": Call PROCESS-FILE-DESCRIPTOR with the resulting ZIP:ZIPFILE
                   Otherwise: Open a file stream and call PROCESS-FILE-DESCRIPTOR with the stream.
  ZIP:ZIPFILE  --- Recall PROCESS-FILE-DESCRIPTOR with a stream for each file in the archive.
  STREAM       --- Simply call STREAM-PROCESSOR.
Otherwise, an error is signalled.")

 (with-file-descriptor
   "Convenience macro for PROCESS-FILE-DESCRIPTOR."))
