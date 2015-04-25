(in-package #:cl-user)
(asdf:defsystem rsbag-helper
  :name "RSBag-Helper"
  :version "1.0.0"
  :license "LGPL2.1"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Helper functions and macros for working with the RSBag utilities."
  :homepage "https://github.com/Shinmera/rsbag-helper/"
  :serial T
  :components ((:file "package")
               (:file "rsbag-helper"))
  :depends-on (:cl-rsbag
               :cl-rsb-common
               :rsbag-tidelog
               :rsb-converter-protocol-buffer
               :uuid
               :zip))
