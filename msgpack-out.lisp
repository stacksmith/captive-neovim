(in-package :cn)
;;==============================================================================
;;
;; MSGPACK encoder
;; https://github.com/msgpack/msgpack/blob/master/spec.md
;;

;;------------------------------------------------------------------------------
;;
;; wb - write an 8-bit octet of integer, optionally starting from startbit.
;;
(defun wb (s int &optional (startbit 0) )
  (write-byte (ldb (byte 8 startbit) int) s))

;;------------------------------------------------------------------------------
;; Encode an integer
(defun !int (s int)
  (if (minusp int)
      (cond;; NEGATIVE numbers!
	((> int -33) (wb s int 0))
	((> int -257) (wb s #xD0) (wb s int))
	((> int -65537)
	 (wb s #xD1) (wb s int 8) (wb s int 0))
	((> int #x-100000001)
	 (wb s #xD2) (wb s int 24) (wb s int 16)(wb s int 8) (wb s int 0))
	(t (wb s #xD3) (wb s int 56) (wb s int 48)(wb s int 40) (wb s int 32)
	   (wb s int 24) (wb s int 16)(wb s int 8) (wb s int 0)))
      (cond;; POSITIVE numbers and 0
	((zerop int) (wb s 0))
	((< int #x80) (wb s int))
	((< int #x100) (wb s #xCC) (wb s int))
	((< int #x10000) (wb s #xCD) (wb s int 8) (wb s int))
	((< int #x100000000)
	 (wb s #xCE) (wb s int 24) (wb s int 16) (wb s int 8) (wb s int))
	(t (wb s #xCE) (wb s int 56) (wb s int 48) (wb s int 40) (wb s int 32)
	   (wb s #xCE) (wb s int 24) (wb s int 16) (wb s int 8) (wb s int))))
  int)

(defun !ints (s &rest ints)
  (loop for int in ints do
       (!int s int)))

;;------------------------------------------------------------------------------
;; string
;;
(defun !string-header (s len)
  (cond
    ((< len #x20) (wb s (+ #xA0 len)))
    ((< len #x100) (wb s #xD9) (wb s len))
    ((< len #x10000) (wb s #xDA) (wb s len 8) (wb s len 0))
    ((< len #x100000000) (wb s #xDB) (wb s len 24) (wb s len 16)(wb s len 8) (wb s len))
    (t (error "String too long")))
  len)

;; TODO: UTF8
(defun !string (s string)
  (!string-header s (length string))
  (loop for c across string do
       (write-byte (char-code c) s)))

;;------------------------------------------------------------------------------
;; just the header - you write the data later!
(defun !array-header (s len)
  (cond
    ((< len #x10) (wb s (+ #x90 len)))
    ((< len #x10000) (wb s #xDC) (wb s len))
    (t (wb s #xDD) (wb s len 24) (wb s len 16)(wb s len 8) (wb s len))))

;;------------------------------------------------------------------------------
(defun !map-header (s len)
  (cond
    ((< len #x10) (wb s (+ #x80 len)))
    ((< len #x10000) (wb s #xDE) (wb s len))
    (t (wb s #xDF) (wb s len 24) (wb s len 16)(wb s len 8) (wb s len))))
