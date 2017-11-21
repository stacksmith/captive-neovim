(in-package :cn)
#||
  This file contains:
- parse trees for character state machines;
- PARSER class containing an array of parse trees, indexed by total length.

A parse tree is a list, containing  peculiar structure representing a number of strings, possibly with common prefixes.  It starts with the common characters representing the common prefix.  The remainder is a list of parse-trees representing the diverging suffixes (which possibly have a 'prefix' in common...).

To search, SEEK matches a character and returns the remaining tree or nil.  At any given point, the remainder may be considered the value of the string matched so far.  

WARNING:

The list of characters runs into the value.  Normally the matching code knows when the search string ends.  However, by looking at the parse tree, there is no way to know what is the search list and what is data.

You are STRONGLY ADVISED to not store data in the form of lists of characters or ALISTS., 
String length is generally the intended way to know what the remaining value is.  Arbitrary values may be stored in the leaf cdr, BUT - if character lists are stored, the search cannot discern between the search string and the payload!


||#

;;------------------------------------------------------------------------------
;; pack - encode (unpack?) a string as a list of characters ending in a . value
(defun pack (name val &optional (start 0) (len (length name)))
  (format t "~&PACKING: ~A.~S ~A~&" start name val)
  (if (zerop len)
      val
      (cons (char name start) (pack name val (1+ start) (1- len)))))

;;------------------------------------------------------------------------------
;; store - add a name in a parse tree; store val
(defun store-p (pos name val &optional (start 0) (len (length name)))
  (labels ((prim (cons)
	     (if (= 1 len)
		  (setf (cdr cons) val)
		  (store-p (cdr cons) name val (1+ start) (1- len)))))
    (let ((it (car pos)) ;; assumption: pos is a cons!
	  (c (char name start)))
      (if (consp it); is car of pos a list?
	  (let ((result (assoc c pos)));; try to find an existing entry for c
	    (if result
		(prim result); as we already have a match,
		(setf (cdr pos) (copy-list pos) ;;no match? split here!
		      (car pos) (pack name val start len))))
	  (if (char= c it)
	      (prim pos)
	      (setf (car pos) (copy-list pos)  
		    (cdr pos) (list (pack name val start len))))))))


;;==============================================================================
;; match a character in a parse tree.  If found, return value; if not, return
;; nil.  
(defun seek (tree char)
  (let ((v (car tree)))
    (typecase v
      (character (when (char= char v)
		   (cdr tree)))
      (cons (cdr (assoc char tree)))
      (t
       (format t "~&seek:fail---~A~&" char)
       nil))))




;;
(defun parse-test (tree string)
  (loop for c across string 
     for q = (seek tree c) then (seek q c)
     finally (return q)))

;;==============================================================================
;; A parser is a datastructure that holds an array of parse-maps by length
;;
(defclass parser ()
  ((arr :accessor arr)))

(defmethod initialize-instance ((parser parser) &key (len 6))
  (setf (arr parser) (make-array (1+ len) :initial-element nil)))
;;------------------------------------------------------------------------------
;; add
(defun parser-add (parser name val)
  (let ((len (length name)))
    (with-slots (arr) parser
      (when (>= len (length arr));; extend the array if necessary
	(setf arr (adjust-array arr (1+ len) :initial-element nil)))
      (if (aref arr len)
	  (store-p (aref arr len) name val 0 len)
	  (setf (aref arr len) (pack name val 0 len))))))

;; store a number of name value parameters pairs...
(defun parser-load (parser list)
  (when list
    (parser-add parser (car list) (cadr list))
    (parser-load parser (cddr list))))

;; a common use case - decode to a symbol
(defun parser-load-symbolically (parser list &key prefix (package *package*))
  (loop for name in list do
       (let ((symname (string-upcase
		       (if prefix
			   (concatenate 'string prefix name)
			   name))))
	 (parser-add parser name
		     (intern symname package)))))
;; start a parse returning a tree.
(defun parser-start (parser length)
  (aref (arr parser) length))



;;==============================================================================
;; Match the string coming from the stream against the parse tree
(defun parse-rpc (parser s)
  (let ((cnt (*header s 'STRING))) ;; header contains string length;
    (loop for i from 0 to cnt
       for q = (parser-start parser cnt) then (seek q (code-char (*U8 s)))
       finally (return q))))

(defun parse-string (parser string)
  (let ((cnt (length string))) ;; header contains string length;
    (loop for c across string
       for q = (seek (parser-start parser cnt) c)
       then (seek q c)
	 finally (return q))))


;; optics

(defun parse-dump-prim (s pos vec cnt)
  (if (zerop cnt)
      (format s "~A: ~A . ~A~&" (length vec) vec pos)
      (typecase pos
	(cons (let ((arg (car pos)))
		(typecase arg
		  (character
		   (vector-push arg vec)
		   (parse-dump-prim s (cdr pos) vec (1- cnt))
		   (vector-pop vec))
		  (cons
		   (loop for a in pos do
			(parse-dump-prim s a vec cnt))))))
	(t (error "pos is not a cons but a ~A" pos))))
  )

(defun parse-dump (parser s)
  (let ((vec (make-array 16 :element-type 'character
			 :adjustable 't :fill-pointer 0)))
    (with-slots (arr) parser
      (loop for slot across arr
	 for i from 0 do
	   (when slot
	     (parse-dump-prim s slot vec i))))))
  
(defmethod print-object ((o parser) s)
  (print-unreadable-object (o s :type t)
    (terpri s)
    (parse-dump o s)))

