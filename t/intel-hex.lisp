#|
  This file is a part of intel-hex project.
  Copyright (c) 2015 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage intel-hex-test
  (:use :cl
        :intel-hex
        :prove))
(in-package :intel-hex-test)

(plan nil)


;;;
;;; test READ-HEX-FROM-FILE function
;;;

(let* ((path (asdf:system-relative-pathname :intel-hex #P"t/test1.hex"))
       (bytes (read-hex-from-file 512 path)))
  (is (aref bytes #x100) #x21)
  (is (aref bytes #x101) #x46)
  (is (aref bytes #x102) #x01)
  (is (aref bytes #x13F) #x21))


(finalize)
