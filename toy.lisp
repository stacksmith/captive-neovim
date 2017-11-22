(in-package :cn)
;; (connect (make-instance 'vimtoy)  32 10)
(defclass vimtoy (nvim)
    ;; keep track of cursor from cursor_goto
  ((w     :accessor w     :initform 0 )
   (h     :accessor h     :initform 0 )
   (x     :accessor x     :initform 0 )
   (y     :accessor y     :initform 0 )
   (cells :accessor cells :initform nil)
   (dirty :accessor dirty :initform nil)
   ) )

(defmethod redraw-start ((vim vimtoy))
  (setf (dirty vim) nil))

(defmethod redraw/resize ((vim vimtoy) width height)
  (with-slots (dirty cells w h) vim
    (setf cells (make-array (list (setf h height)
				  (setf w width)))
	  dirty t)))

(defmethod redraw/clear ((vim vimtoy))
  (with-slots (dirty cells w h) vim
    (loop for y from 0 below h do
	 (loop for x from 0 below w do
	      (setf (aref cells y x) #\space)))
    (setf dirty t)))

(defmethod redraw/eol_clear ((vim vimtoy))
  (with-slots (dirty cells x y w h) vim
    (loop for %x from x below w do
	 (setf (aref cells y %x) #\space)
       finally (setf x %x))
    (setf dirty t)))

(defmethod redraw/goto ((vim vimtoy) row column)
  (with-slots (x y) vim
    (setf y row
	  x column )))

(defmethod redraw/put ((vim vimtoy) s count)
  (with-slots (dirty x y cells) vim
    (loop for i from 0 below count
       for xpos from x do
	 (let ((cell-cnt (*header s 'ARRAY 1)))
	   (setf (aref cells y xpos) (char (*string s) 0))))
    (setf dirty t)))

(defmethod redraw-end ((vim vimtoy))
  (with-slots (dirty cells w h) vim
    (when dirty
      (dotimes (i w) (format t "=" ))
      (terpri)
      (loop for y from 0 below h do
	   (loop for x from 0 below w do
		(format t "~A" (aref cells y x)))
	   (terpri))
      (dotimes (i w) (format t "=" ))
      (setf dirty nil))
    ))

(defmethod redraw/color ((vim vimtoy) what color)
)
(defmethod redraw/highlight ((vim nvim) fore back spec
				 reve ital bold line curl)
)

(defun ! ( keys &optional (vim *v*))
  (let ((barf (!command vim "nvim_input" 1)))
    (!string barf keys)
    (force-output barf)))
