(in-package :cn)
;;==============================================================================
;; MODE_INFO_SET message
;;
;; Create modes as directed.  Place resultant mode instances into an *modes*
;;
;; Note: As it stands, we create a new mode array every time this happens.  I
;; don't know how often this happens yet.  Strings are consed up for name and
;; short_name elements.
;;
;; TODO: a generic to be able to hook in when this happens?
;;
(defparameter *parser-mis* nil);mode_info_set
(defparameter *parser-cursor-shapes*
  (make-instance 'parser :len 10
		 :symbolically '("block" "horizontal" "vertical")))
;; array of modes, indexed by mode number
(defparameter *modes* nil)

;; to build a proper map we need to convert keys to symbols, check data type.
;;==============================================================================
;; each field will parse to a pair containing slot name and a function to get
;; value for slot...
(defun init-parser-mis ()
  ;; return a symbol for the highlight type string...
  (setf *parser-mis* (make-instance 'parser :len 10))
  (parser-load
   *parser-mis* 
   `( "mouse_shape" ,(lambda (s) (values 'mouse_shape (*int s)))
		    "cursor_shape"
      ,(lambda (s) (values 'cursor_shape
			   (parse-rpc *parser-cursor-shapes* s)))
      "cell_percentage"  ,(lambda (s) (values 'cell_percentage (*int s)))
      "blinkwait"  ,(lambda (s) (values 'blinkwait (*int s)))
      "blinkon"    ,(lambda (s) (values 'blinkon   (*int s)))
      "blinkoff"   ,(lambda (s) (values 'blinkoff  (*int s)))
      "name"       ,(lambda (s) (values 'name (*string s)))
      "short_name" ,(lambda (s) (values 'short_name (*string s)))
      "hl_id"      ,(lambda (s) (values 'hl_id  (*int s)))
      "id_lm"      ,(lambda (s) (values 'id_lm  (*int s))))))

(defclass mode-info ()
  ((mouse_shape :accessor mouse_shape :initform nil)
   (cursor_shape :accessor cursor_shape :initform nil)
   (cell_percentage :accessor cell_percentage :initform nil)
   (blinkwait :accessor blinkwait :initform nil)
   (blinkon :accessor blinkon :initform nil)
   (blinkoff :accessor blinkoff :initform nil)
   (name :accessor name :initform nil)
   (short_name :accessor short_name :initform nil)
   (hl_id :accessor hl_id :initform nil)
   (id_lm :accessor id_lm :initform nil)   ))

(defmethod print-object ((o mode-info) s)
  (print-unreadable-object (o s :type t)
    (let ((slots '(name short_name
		   mouse_shape cursor_shape cell_percentage
		   blinkwait blinkon blinkoff
		   hl_id id_lm)))
      (print (mapcar (lambda (slot)
		       (cons slot
			     (slot-value o slot)))
		     slots)
	     s))))
;;------------------------------------------------------------------------------
;; Create and set the mode array, containing mode-info instances for each mode.
;;
(defun %redraw/mode_info_set (vim s data-cnt)
  ;; Expecting an array with 'cursor-style-enabled' and data
  (*header s 'ARRAY 2)
  (let ((cursor-style-enabled (*bool s))
	(mode-cnt (*header s 'ARRAY )))
    (setf *modes* (make-array mode-cnt))
    (loop for i from 0 below mode-cnt 
       for mode = (make-instance 'mode-info); create a new mode
       for element-cnt = (*header s 'MAP) do; count of slots we shall set
	 (loop for e from 0 below element-cnt do; and for every element 
	      (multiple-value-bind (slot-name val); parser returns
		  (funcall (parse-rpc *parser-mis* s) s)       
		(setf (slot-value mode slot-name) val)))
	 (setf (aref *modes* i) mode))))

