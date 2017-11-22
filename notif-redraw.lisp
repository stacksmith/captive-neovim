(in-package :cn)
#||
 Strategy:

Since messagepack provides a unique cons-free RPC mechanism, it is a shame to
use it simply to cons up strings and arrays, just to through them out after
parsing.

We therefore use a static parse-tree, a kind of 'keymap' to navigate certain
parts of messgapack transmission.  See parse-tree.lisp for more details.

So the top function, on-notif-redraw, dispatches to the %-prefixed functions
via the parse-tree; the % functions in turn call the methods shown below with
proper lisp parameters.

||#

(defparameter *parser-redraw* nil)
(defparameter *parser-highlight* nil)

;;==============================================================================
;; REDRAW protocol overrides
(defgeneric redraw/resize (vim width height))
(defgeneric redraw/clear  (vim))
(defgeneric redraw/eol_clear  (vim))
(defgeneric redraw/resize (vim width height))
(defgeneric redraw/color (vim what color)); what is :fg :bg or :sp
(defgeneric redraw/highlight (vim fg bg sp reve ital bold line curl))
(defgeneric redraw/goto (vim row column))
(defgeneric redraw/put (vim s count))

(defgeneric redraw-start (vim)) ;; not a
(defgeneric redraw-end   (vim)) ;; not a 

;;------------------------------------------------------------------------------
(defun check-cnt (exp act)
   (unless (= exp act)
	 (error "Expected ~A data item, not ~A" exp act)))
;;------------------------------------------------------------------------------
;; on-notif-redraw   DISPATCH!
;;

(defmethod on-notif-redraw ((vim nvim) s data-cnt)
  (check-cnt 1 data-cnt)
  ;; receive an array of tuples, and dispatch
  (let ((tuple-count (*header s 'ARRAY)))
    ;;  (format t "~&~A tuples:~&" tuple-count)
    (redraw-start vim)
    (loop for i from 0 below tuple-count
       for item-cnt = (1- (*header s 'ARRAY))
       for handler = (parse-rpc *parser-redraw* s) do
;;;	 (format t "~&:::~A:::~&" i)
	 (if handler
	     (funcall handler vim s item-cnt)
	     (progn
	       (format t "handler not found (~A)~&" item-cnt)
	       (loop for i from 0 below item-cnt do
		    (print (explore s))))))
    (redraw-end vim)
 ;;   (format t "NOTIF-REDRAW DONE~&")
    ))


;;------------------------------------------------------------------------------
;;
;;
(defun %redraw/resize (vim s data-cnt)
  (check-cnt 1 data-cnt)
  (*header s 'ARRAY 2)
  (redraw/resize vim (*int s) (*int s)))

(defmethod redraw/resize ((vim nvim) width height)
  (format t "~&:resize: ~A ~A~&" width height))

;;------------------------------------------------------------------------------
(defun %redraw/clear (vim s data-cnt )
  (check-cnt 1 data-cnt)
  (*header s 'ARRAY 0)
  (redraw/clear vim))

(defmethod redraw/clear ((vim nvim))
  (format t "~&:clear~&"))

;;------------------------------------------------------------------------------
(defun %redraw/eol_clear (vim s data-cnt )
  (check-cnt 1 data-cnt)
  (*header s 'ARRAY 0)
  (redraw/eol_clear vim))

(defmethod redraw/eol_clear ((vim nvim))
  (format t "~&:eol_clear~&"))

;;==============================================================================
;; The color updates all invoke the same update-color method:
(defmethod redraw/color ((vim nvim) what color)
  (format t "~&:update ~A: ~A ~&" what color))

;;------------------------------------------------------------------------------
(defun %redraw/update_fg (vim s data-cnt)
  (check-cnt 1 data-cnt)
  (*header s 'ARRAY 1)
  (redraw/color vim :fg (*int s)))

;;------------------------------------------------------------------------------
(defun %redraw/update_bg (vim s data-cnt )
  (check-cnt 1 data-cnt)
  (*header s 'ARRAY 1)
  (redraw/color vim :bg (*int s)))

;;------------------------------------------------------------------------------
(defun %redraw/update_sp (vim s data-cnt )
  (check-cnt 1 data-cnt)
  (*header s 'ARRAY 1)
  (redraw/color vim :sp (*int s)))



;;==============================================================================
;; Highlight           https://neovim.io/doc/user/ui.html#ui-event-highlight_set
;;
;; 8 highlight subcommands.  First 3 provide colors; rest are booleans.
;;
(defun init-parser-highlight ()
  ;; return a symbol for the highlight type string...
  (setf *parser-highlight* (make-instance 'parser :len 10))
  (parser-load-symbolically
   *parser-highlight*
   '("foreground" "background"   "special"   "reverse"
     "italic"     "bold"         "underline"  "undercurl" )
   :prefix "HL/"))
;;------------------------------------------------------------------------------
;; highlight: 3 colors and 5 boolean keys...
(defun %redraw/highlight_set (vim s data-cnt)
  (let (fore back spec reve ital bold line curl)
;;    (format t "HIGH CNT ~A~&" data-cnt)
    (case data-cnt
      (1
       (*header s 'ARRAY 1)
       (explore s))
      (2
       (*header s 'ARRAY 1)  
       (explore s)
       (*header s 'ARRAY 1)
       (let ((cnt (*header s 'MAP)))
	 (loop for i from 0 below cnt do
	      (case (parse-rpc *parser-highlight* s)
		(hl/foreground (setf fore (*int s)))
		(hl/background (setf back (*int s)))
		(hl/special    (setf spec (*int s)))
		(hl/reverse    (setf reve (*bool s)))
		(hl/italic     (setf ital (*bool s)))
		(hl/bold       (setf bold (*bool s)))
		(hl/underline  (setf line (*bool s)))
		(hl/undercurl  (setf curl (*bool s)))
		(t (error "Highlight is not known.")))))))
    
    (redraw/highlight vim fore back spec
			  reve ital bold line curl)))

(defmethod redraw/highlight ((vim nvim) fore back spec
				 reve ital bold line curl)
  (format t "~&:highlight: ~A ~A ~A
  reve:~A ital:~A bold:~A line:~A curl:~A~&" fore back spec
  reve ital bold line curl  ))

;;==============================================================================
;; goto
(defun %redraw/cursor_goto (vim s data-cnt)
  (check-cnt 1 data-cnt)
  (*header s 'ARRAY 2)
  (redraw/goto vim (*int s) (*int s)))
(defmethod redraw/goto ((vim nvim) row column)
  (format t "~&:-goto: row ~A col ~A~&" row column))
;;==============================================================================
;; put
(defun %redraw/put (vim s data-cnt)
  (redraw/put vim s data-cnt))
;;------------------------------------------------------------------------------
;; default
(defmethod redraw/put ((vim nvim) s data-cnt)
  (format t "~&put: ")
  (loop for i from 0 below data-cnt do
       (let ((cell-cnt (*header s 'ARRAY 1)))
	 (format t "~S" (*string s) )))
  (terpri))
;;==============================================================================
;; modes_info_set is in its own file mode-info-set.lisp

;;------------------------------------------------------------------------------
;; TODO: build a parser

;;------------------------------------------------------------------------------
;; TODO:
(defun %redraw/mode_change (vim s data-cnt)
  (*header s 'ARRAY 2)
  (let ((mode-name (*string s))
	(mode-index (*int s)))))
;;------------------------------------------------------------------------------
(defmethod redraw-start ((vim nvim))
  )
(defmethod redraw-end ((vim nvim))
  )
;;==============================================================================
;; initialize the redraw handlers - once
;;
(defun init-parser-redraw ()
  (setf *parser-redraw* (make-instance 'parser :len 13))
  (parser-load
   *parser-redraw*
   '("resize"      %redraw/resize
     "clear"       %redraw/clear
     "eol_clear"   %redraw/eol_clear
     "cursor_goto" %redraw/cursor_goto
     "update_fg"   %redraw/update_fg
     "update_bg"   %redraw/update_bg
     "update_sp"   %redraw/update_sp
     "highlight_set" %redraw/highlight_set
     "put"         %redraw/put
     "mode_info_set" %redraw/mode_info_set
     "mode_change" %redraw/mode_change)))

