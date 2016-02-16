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


;;
;; test READ-START-CODE function

(diag "READ-START-CODE")

(with-input-from-string (stream "foo")
  (is-error (intel-hex::read-start-code stream) 'simple-error
            "Unexpected character."))


;;
;; test READ-NEWLINE function

(diag "READ-NEWLINE")

(with-input-from-string (stream "foo")
  (is-error (intel-hex::read-newline stream) 'simple-error
            "Unexpected character."))

;;
;; test VERIFY-CHECK-SUM function

(diag "VERIFY-CHECK-SUM")

(with-input-from-string (stream ":0100000000FE")
  (let ((intel-hex::*check-sum* 0))
    (intel-hex::read-start-code stream)
    (intel-hex::read-word8 stream t)
    (intel-hex::read-word8 stream t)
    (intel-hex::read-word8 stream t)
    (intel-hex::read-word8 stream t)
    (intel-hex::read-word8 stream t)
    (is-error (intel-hex::verify-check-sum stream) simple-error
              "Unmatched check sum.")))


;;
;; test READ-HEX function

(diag "READ-HEX")

(with-input-from-string (stream ":020000021200EA")
  (is-error (read-hex 256 stream) simple-error
            "Record type '02' not implemented."))

(with-input-from-string (stream ":0400000300003800C1")
  (is-error (read-hex 256 stream) simple-error
            "Record type '03' not implemented."))

(with-input-from-string (stream ":04000005000000CD2A")
  (is-error (read-hex 256 stream) simple-error
            "Record type '05' not implemented."))


;;
;; test READ-HEX-FROM-FILE function

(diag "READ-HEX-FROM-FILE")

(let* ((path (asdf:system-relative-pathname :intel-hex #P"t/test1.hex"))
       (bytes (read-hex-from-file 512 path)))
  (is (aref bytes #x100) #x21 "The first byte of the array is 0x21.")
  (is (aref bytes #x101) #x46 "The second byte of the array is 0x46.")
  (is (aref bytes #x102) #x01 "The thrid byte of the array is 0x01.")
  (is (aref bytes #x13F) #x21 "The last byte of the array is 0x21."))


;;
;; test READ-HEX-FROM-STRING function

(diag "READ-HEX-FROM-STRING")

(let ((bytes (read-hex-from-string 256 ":00000001FF")))
  (is (aref bytes #x00) #x00 "The first byte of the array is 0x00."))


;;
;; test WRITE-HEX-LINE

(diag "WRITE-HEX-LINE")

(is (remove #\NewLine
     (with-output-to-string (out)
       (intel-hex::write-hex-line out #(02 #x33 #x7a) 0 0 3 #x30)))
    ":0300300002337A1E")


;;
;; test WRITE-HEX-LINE errors

(is-error (intel-hex::write-hex-line 'unused 'unused #x10000 1 0)
	  'simple-error
	  "Starting address too long")

(is-error (intel-hex::write-hex-line 'unused 'unused 0 -1 0)
	  'simple-error
	  "Non-existent type code")

(is-error (intel-hex::write-hex-line 'unused 'unused 0 3 0)
	  'simple-error
	  "Unsupported type code")

(with-output-to-string (out)
  (is-error (intel-hex::write-hex-line out #(02 #x33 #x7af) 0 0 3 #x30)
	    'simple-error "Too long octet"))


;;
;; test WRITE-HEX-TO-FILE

(diag "WRITE-HEX-TO-FILE")

(uiop:with-temporary-file (:pathname pathname)
  (write-hex-to-file #1=#(10 11 23 45 32 94) pathname :if-exists :supersede)
  (is (read-hex-from-file 6 pathname)
      #1#
      :test #'equalp
      "Ok. - simple"))

(uiop:with-temporary-file (:pathname pathname)
  (write-hex-to-file '(#x100 #1=#(10 11 23 45 32 94)) pathname
                     :if-exists :supersede)
  (is (subseq (read-hex-from-file #x106 pathname) #x100)
      #1#
      :test #'equalp
      "Ok. - with offset"))

(uiop:with-temporary-file (:pathname pathname)
  (write-hex-to-file #(#xabcd #xbeef) pathname
                     :if-exists :supersede
                     :vector-size 2)
  (is (read-hex-from-file 4 pathname)
      #(#xcd #xab #xef #xbe)
      :test #'equalp
      "Ok. - doubled"))


;;
;; test WRITE-HEX-TO-STRING

(diag "WRITE-HEX-TO-STRING")

(is (read-hex-from-string 6 (write-hex-to-string #1=#(10 11 23 45 32 94)))
    #1#
    :test #'equalp
    "Ok. - simple")

(is (subseq
     (read-hex-from-string #x106
      (write-hex-to-string '(#x100 #1=#(10 11 23 45 32 94))))
     #x100)
    #1#
    :test #'equalp
    "Ok. - with offset")

(is (read-hex-from-string 4 (write-hex-to-string #(#xabcd #xbeef)
                                                 :vector-size 2))
    #(#xcd #xab #xef #xbe)
    :test #'equalp
    "Ok. - doubled")

(finalize)
