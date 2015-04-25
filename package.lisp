(in-package #:cl-user)
(defpackage #:rsbag-helper
  (:use #:cl)
  (:export
   #:*rst-path*
   #:load-proto-file
   #:make-precise-timestamp
   #:ensure-keyword
   
   #:bag-channel
   #:bag
   #:channel
   #:id
   #:scope
   #:type
   
   #:to-vicon-object
   #:*sequence-number*
   #:make-entry
   #:make-default-transform
   #:with-default-bag
   #:with-bag-channel
   #:process-file-descriptor
   #:with-file-descriptor))
