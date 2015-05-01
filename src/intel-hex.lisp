#|
  This file is a part of intel-hex project.
  Copyright (c) 2015 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage intel-hex
  (:use :cl)
  (:export #:read-hex
           #:read-hex-from-file
           #:read-hex-from-string))
(in-package :intel-hex)

(defvar *check-sum* 0)

(defun read-start-code (stream)
  (let ((c (read-char stream)))
    (unless (char= c #\:)
      (error "Unexpected character: ~A" c)))
  t)

(defun read-newline (stream)
  (let ((c (read-char stream)))
    (unless (char= c #\Newline)
      (error "Unexpected character: ~A" c)))
  t)

(defun read-word8 (stream &optional check-sum-p)
  (let ((c0 (read-char stream))
        (c1 (read-char stream)))
    (let ((w (read-from-string (concatenate 'string (list #\# #\x c0 c1)))))
      (when check-sum-p
        (incf *check-sum* w))
      w)))

(defun read-word16 (stream &optional check-sum-p)
  ;; 16-bit words are always expressed as big endian values in Intel HEX format.
  (let ((w0 (read-word8 stream check-sum-p))
        (w1 (read-word8 stream check-sum-p)))
    (+ (ash w0 8) w1)))

(defun verify-check-sum (stream)
  (let ((check-sum (read-word8 stream nil)))
    (unless (= (logand (+ *check-sum* check-sum) #xff) #x00)
      (error "Unmatched check sum.")))
  (setf *check-sum* 0)
  t)

(defun read-hex (size &optional (stream *standard-input*))
  (let ((*check-sum* 0)
        (ret (make-array size :element-type '(unsigned-byte 8)))
        (ela 0)                         ; Extended linear address
        byte-count address record-type eof)
    (loop until eof
       do ;; Read start code.
          (read-start-code stream)
          ;; Read byte count.
          (setf byte-count (read-word8 stream t))
          ;; Read address.
          (setf address (read-word16 stream t))
          ;; Read record type.
          (setf record-type (read-word8 stream t))
          ;; Read the rest of a record.
         (ecase record-type
           (#x00 ;; Read data.
                 (loop repeat byte-count
                       for i from 0
                    do (let ((address1 (+ ela address i)))
                         (setf (aref ret address1) (read-word8 stream t))))
                 ;; Verify check sum.
                 (verify-check-sum stream)
                 ;; Read new line.
                 (read-newline stream))
           (#x01 ;; Verify check sum.
                 (verify-check-sum stream)
                 ;; Set EOF flag.
                 (setf eof t))
           (#x02 (error "Record type '02' not implemented."))
           (#x03 (error "Record type '03' not implemented."))
           (#x04 ;; Read extended linear address.
                 (setf ela (ash (read-word16 stream t) 16))
                 ;; Verify check sum.
                 (verify-check-sum stream)
                 ;; Read new line.
                 (read-newline stream))
           (#x05 (error "Record type '05' not implemented."))))
    ret))

(defun read-hex-from-file (size filename)
  (with-open-file (stream filename :direction :input)
    (read-hex size stream)))

(defun read-hex-from-string (size string)
  (with-input-from-string (stream string)
    (read-hex size stream)))
