(in-package :cn)
;;==============================================================================
;;
;; NeoVIM UI connection
;;
;; https://neovim.io/doc/user/ui.html
;;
;; (connect (make-instance 'nvim) 30 10)
(defparameter *v* nil)
(defparameter *in* nil)

(defparameter *parser-noti* nil)
;; A more sophisticated client can track commands sent to NVIM by sequence id,
;; and request callbacks on reply (possibly, only on error?)
;; A simple one to start with does nothing..
(defclass seq-tracker ()
  ((seq     :accessor seq :initform 0)
))

(defmethod get-seq ((tracker seq-tracker) &key (callback nil) (always nil))
  (declare (ignore callback always))
  (let ((seq (seq tracker)))
    (incf (seq tracker))
    seq))

(defclass nvim ()
  ((process :accessor process)
   (slurp   :accessor slurp)
   (barf    :accessor barf)
   (tracker :accessor tracker ) ;; sequence tracking
   ;; 
))

(defmethod initialize-instance ((vim nvim) &key)
)

(defmethod get-seq ((vim nvim) &key (callback nil) (always nil))
  (get-seq (tracker vim) :callback callback :always always))

(defun connect (vim rows cols)
  (with-slots (process slurp barf) vim
    (setf process (external-program:start
		   "nvim" '("--embed") :input :stream :output :stream)
	  slurp  (external-program:process-output-stream process)
	  barf   (external-program:process-input-stream process))

    (setf (tracker vim) (make-instance 'seq-tracker))
    
    (nvim-ui-attach vim rows cols)

    (setf *v* vim
	  *in* slurp)))


;;------------------------------------------------------------------------------
;;==============================================================================
;; Main input mechanism.
;; Input and process a complete response from VIM.
;; Return nil if no input was waiting, or t if processed.
(defun in (vim)
  (let ((s (slurp vim)))
    (when (listen s) ;;if nothing to report, nil...
      (let ((data-cnt  (1- (*header s 'ARRAY))) ;;expecting array
	    (reply-type (*int s)))
	(case reply-type
	  (1 (on-response vim s data-cnt))
	  (2 (on-notification vim s data-cnt)
	     ;;	     (format t "IN SHOULD BE DONE ~&")
	     )
	  (t (error "read: unknow reply ~A: ~A ~A "
		    reply-type (explore s) (explore s)))))
      t)))


(defun updates (vim)
  (loop while (in vim)))

;;==============================================================================
;; The kinds of answers are handled here.
;;
;; We read the reply-type.
;;------------------------------------------------------------------------------
;; Default response handler ignores
(defmethod on-response ((vim nvim) s data-count)
;;  (format t "ON-RESPONSE ~A items~&" data-count)
  (let ((seq (*int s))
	(e (explore s))
	(result (explore s)))
    (when e (error "seq-reply error on packet ~A: ~A ~A"
		   seq e result))))

(defmethod on-notification ((vim nvim ) s data-cnt)
  (let ((handler (parse-rpc *parser-noti* s)))
    (if handler
	(funcall handler vim s (1- data-cnt))
	(format t "Unhandled notification"))
	))
  ;;
  #||
  (let ((notification-kind (*string s)))
    (cond
      ((string= "redraw" notification-kind) (on-notif-redraw vim s (1- data-cnt))) 
      (t
       (format t "Unhandled notification ~A" notification-kind))
      ))
||#  


;;==============================================================================
;; OUTGOING PROTOCOL COMMANDS
;;
(defun nvim-ui-attach (vim rows cols &optional )
  (let ((s (!command vim "nvim_ui_attach" 3)))
    (!ints s rows cols)
    (!map-header s 0)
    (force-output s)))


(defun %redraw (vim s data-cnt)
  (on-notif-redraw vim s data-cnt ))

;; notification kinds are encoded as strings...
(defun init-parser-notification ()
  (setf *parser-noti* (make-instance 'parser :len 13))
  (parser-load *parser-noti*
	       '("redraw" %redraw)))




;;------------------------------------------------------------------------------
;;
;; start command header, optionally requesting a when-done callback (if error
;;
(defun !command (vim command paramcnt &key (callback nil) (always nil))
  (let ((s (barf vim)))
    (!array-header s 4) ;; command array
    (!ints s 0 (get-seq vim :callback callback :always always))
    (!string s command)
    (!array-header s paramcnt)
    s))


;; global initialization
(defun init ()
  (init-parser-notification)
  (init-parser-redraw )
  (init-parser-highlight)
  (init-parser-mis) ;mode-info-set
  (interpol:enable-interpol-syntax) 
  )
