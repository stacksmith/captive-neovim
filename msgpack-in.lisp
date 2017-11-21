(in-package cn)
;;==============================================================================
;; MSGPACK decoder
;; https://github.com/msgpack/msgpack/blob/master/spec.md
;;
;;------------------------------------------------------------------------------
(defun skip-bytes (stream count)
  (dotimes (i count) (read-byte stream)))

;;==============================================================================
;; Basic stream reader *U8 and *U16 are macros
(defmacro *U8 (stream)
     `(the (unsigned-byte 8) (read-byte ,stream)))

(defmacro *U16 (stream)
  `(the fixnum (+ (ash (*U8 ,stream) 8)
		 (*U8 ,stream))))

(defun *U32 (stream)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (+ (ash (*U16 stream) 16)
     (*U16 stream)))

(defun *U64 (stream)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (+ (ash (*U32 stream) 32)
     (*U32 stream)))

;;------------------------------------------------------------------------------
;; Signed integers
;;
(defun *S8 (stream)
  (let ((val (*U8 stream)))
    (if (< val #x80)
	val
	(- val #x80))))

(defun *S16 (stream)
  (let ((val (*U16 stream)))
    (if (< val #x8000)
	val
	(- val #x8000))))

(defun *S32 (stream)
  (let ((val (*U32 stream)))
    (if (< val #x80000000)
	val
	(- val #x80000000))))

(defun *S64 (stream)
  (let ((val (*U64 stream)))
    (if (< val #x8000000000000000)
	val
	(- val #x8000000000000000))))

;;------------------------------------------------------------------------------
;;
;; DECODER
;;
;; Return TYPE and VALUE
;;
;; For integers, value is the actual decoded integer.
;;
;; For aggregate types (strings, arrays, etc), value is the number of elements
;; to follow.
;;
(defun *decode (stream)
  (let ((byte (*U8 stream)))
  ;; (format t "BYTE ~X~&" byte)
     (cond 
       ((< byte #x80) (values 'INTEGER byte))
       ((< byte #x90) (values 'MAP    (ldb (byte 4 0) byte)))
       ((< byte #xA0) (values 'ARRAY  (ldb (byte 4 0) byte)))
       ((< byte #xC0) (values 'STRING (ldb (byte 5 0) byte)))
       ((< byte #xE0)
	(case byte
	 (#xC0 (values 'NULL nil))
	 (#xC1 (error "Code C1"))
	 (#xC2 (values 'BOOLEAN nil)) ;;boolean...
	 (#xC3 (values 'BOOLEAN t))
	 (#xC4 (values 'BIN (*U8 stream))) ;; BIN8 (byte array)
	 (#xC5 (values 'BIN (*U16 stream)));; BIN16
	 (#xC6 (values 'BIN (*U32 stream)));; BIN32
	 (#xC7 (values 'EXT (*U8 stream)));; EXT8
	 (#xC8 (values 'EXT (*U16 stream)));; EXT16
	 (#xC9 (values 'EXT (*U32 stream)));; EXT32
	 (#xCA (error "FLOAT32"))
	 (#xCB (error "FLOAT64"))
	 (#xCC (values 'INTEGER (*U8 stream)))
	 (#xCD (values 'INTEGER (*U16 stream)))
	 (#xCE (values 'INTEGER (*U32 stream)))
	 (#xCF (values 'INTEGER (*U64 stream)))
	 (#xD0 (values 'INTEGER (*S8 stream)))
	 (#xD1 (values 'INTEGER (*S16 stream)))
	 (#xD2 (values 'INTEGER (*S32 stream)))
	 (#xD3 (values 'INTEGER(*S64 stream)))
	 (#xD4 (values 'EXT 1));;FIXEXT1
	 (#xD5 (values 'EXT 2))
	 (#xD6 (values 'EXT 4))
	 (#xD7 (values 'EXT 8))
	 (#xD8 (values 'EXT 16))
	 (#xD9 (values 'STRING (*U8 stream)))
	 (#xDA (values 'STRING (*U16 stream)))
	 (#xDB (values 'STRING (*U32 stream)))
	 (#xDC (values 'ARRAY (*U16 stream)))
	 (#xDD (values 'ARRAY (*U32 stream)))
	 (#xDE (values 'MAP (*U16 stream)))
	 (#xDF (values 'MAP (*U32 stream)))))
       (t (values 'INTEGER (- (ldb (byte 5 0) byte) 32))))))

;;------------------------------------------------------------------------------
;;
;; Often we expect an aggregate object of a known type, and sometimes, of a
;; certain size.  In such a case, *header is used to check the type (and
;; optionally size), and return the actual size.
(defun *header (s expected-type &optional expected-count)
  (multiple-value-bind (type count) (*decode s)
    (unless (eq type expected-type)
      (error "~A expected; got ~A ~A" expected-type type count))
    (when expected-count
      (unless (= expected-count count)
	(error "~A of ~A expected, but count is ~A"
	       expected-type expected-count count)))
    count))
;;------------------------------------------------------------------------------
;; After parsing the header, make a Lisp string if you so desire:
(defun *string-data (stream size)
  (let ((result (make-string size)))
    (loop for i from 0 below size do
	 (setf (aref result i) (code-char (*U8 stream))))
    result))
(defun *string (s &optional expected-count)
  (*string-data s (*header s 'STRING expected-count)))
;;------------------------------------------------------------------------------
;; Array and map do not belong here, if you think about it (data types unknown)

(defun *int (s)
  (multiple-value-bind (type value) (*decode s)
    (unless (eq type 'INTEGER) (error "Integer expected; got ~A ~A" type value))
    value))

(defun *bool (s)
  (multiple-value-bind (type value) (*decode s)
    (unless (or (eq type 'BOOLEAN)
		(eq type 'NIL))
	(error "Boolean expected; got ~A ~A" type value))
    value))



;;------------------------------------------------------------------------------
;; Explorer - just read as sexps...
(defun explore (stream)
  (multiple-value-bind (type value) (*decode stream)
    (case type
      (INTEGER value)
      (STRING  (*string-data stream value))
      (BOOLEAN value)
      (NULL     value)
      (ARRAY
       (make-array value :initial-contents
		   (loop for i from 0 below value
		      collect (explore stream))))
      (MAP
       (loop for i from 0 below value
	    collect (cons (explore stream) (explore stream)) ))
      (t (format t "UNIMPLEMENTED ~A ~A~&" type value)))))
