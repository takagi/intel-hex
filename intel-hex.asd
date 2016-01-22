#|
  This file is a part of intel-hex project.
  Copyright (c) 2015 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage intel-hex-asd
  (:use :cl :asdf))
(in-package :intel-hex-asd)

(defsystem intel-hex
  :version "0.1"
  :author "Masayuki Takagi"
  :license "MIT"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "write" :depends-on ("package"))
                 (:file "read" :depends-on ("package")))))
  :description "A library to handle Intel HEX format."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op intel-hex-test))))
