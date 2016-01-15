;;;; Copyright (C) 2012, 2015, 2016 by Tomas Zellerin

(in-package intel-hex)

(defvar *default-chunk-size* #x10
  "How many octets should go to one hex line.")

(define-modify-macro update-checksum (octet)
  (lambda (old-checksum octet)
    (declare ((unsigned-byte 8) old-checksum octet))
    (setf checksum (mod (- old-checksum octet) #x100)))
  "Update value of checksum by a new octet.")

(defun write-hex-line (out data start type length &optional
			    (offset 0))
  "Print one line of data in hex format to a stream.

For type 00, LENGTH bytes starting at address START of vector DATA is written as
data at address START+OFFSET.

Type 01 is end of file record. The byte count is 00 and the data field
is empty."
  (assert (<= 0 (+ offset start) #xFFFF) ()
	  "Only 16bit start addresses are suported, not ~x" start)
  (assert (<= 0 type 5) () "Non-existent type code ~d" type)
  (assert (<= 0 type 1) () "Unsupported type code ~d" type)
  (when (= type 1)
    (assert (zerop length) () "Non-zero length of e-o-f record"))
  (format out "~&:~2,'0x" length)      ; prefix and byte count/size
  (format out "~4,'0x" (+ offset start))  ; 16 bit address
  ;;;; Record type, two hex digits, 00 to 05, defining the type of the data field.
  (format out "~2,'0x" type) ;
  ;; Data, a sequence of n bytes of the data themselves, represented by 2n hex digits.
  (loop
     with declared-start = (+ start offset)
     with checksum = 0
     for i from start below (+ start length)
     for val = (aref data i)
     initially
       (update-checksum checksum length)
       (update-checksum checksum type)
       (update-checksum checksum (ldb (byte 8 0) declared-start))
       (update-checksum checksum (ldb (byte 8 8) declared-start))
     do
       (assert (<= 0 val #xff))
       (format out "~2,'0x" val)
       (update-checksum checksum val)
     finally
       ;; check sum and end line
       (format out "~2,'0x~%" checksum)))

(defun write-hex-single-vector (data &optional (stream *terminal-io*) (offset 0)
			 (chunk-size *default-chunk-size*))
  "Print vector of octets DATA to the output stream STREAM as lines, each
having up to CHUNK-SIZE octets. The address in the hex files differs
from position in DATA by OFFSET.

Do not print the end-of-file line."
  (loop
     for start from 0 upto (1- (length data)) by chunk-size
     do
       (write-hex-line stream data start
			    0
			    (min chunk-size (- (length data) start))
			    offset)))

(defun code-to-octets (byte-size-in-octets data)
  "Convert 16bit or 32 bit vector DATA to twice or four times as long
vector of octets."
  (loop
    with res = (make-array (* byte-size-in-octets (length data))
			   :element-type '(unsigned-byte 8)
			   :fill-pointer 0)
     for orig across data
     for i from 0 by byte-size-in-octets
     do
       (loop for octet from 0 to (1- byte-size-in-octets)
	  do
	    (vector-push  (ldb (byte 8 (* 8 octet)) orig) res))
     finally
       (return res)))

(defun write-hex (offset-and-data &optional (out *standard-output*)
					    (chunk-size *default-chunk-size*)
					    vector-size)
"OFFSET-AND-DATA is a list of alternating initial addresses and vectors
to be put at the address; as special case, a single vector (assumed to
start at address 0) can be also used.

OPS may contain additional parameters; currently:
- :chunk-size is supported to change size of individual lines,
- :vector-size can be used to store more than one octet data (e.g., Microchip
  tools use 16bit for their midrange processors, so use 2 here).
  Start addresses are multiplied by the factor as well (is this correct behaviour?)."
  (flet ((converted (data)
	      (if vector-size
		  (code-to-octets vector-size data)
		  data)))
	(if (atom offset-and-data)
	    (write-hex-single-vector (converted offset-and-data) out 0 chunk-size)
	    (loop
	      for (offset data) on offset-and-data
	      by #'cddr
	      do (write-hex-single-vector (converted data)
					  out  (* (or vector-size 1) offset) chunk-size))))
  (write-hex-line out #() 0 1 0)
  (force-output out))

(defun write-hex-to-file (offset-and-data file &key (chunk-size *default-chunk-size*)
						 vector-size (if-exists :supersede))
  "Print intel hex representation of one or multiple vectors to
FILE.

Parameters are analogous to WRITE-HEX; in addition, IF-EXISTS is
passed to OPEN call."
  (with-open-file (out file :direction :output
		       :if-exists if-exists)
    (write-hex offset-and-data out chunk-size vector-size)))

(defun write-hex-to-string (offset-and-data &key (chunk-size *default-chunk-size*)
						 vector-size)
  "Print intel hex representation of one or multiple vectors to a string.

Parameters are analogous to WRITE-HEX."
  (with-output-to-string (out)
    (write-hex offset-and-data out chunk-size vector-size)))
