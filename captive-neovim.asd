;;
;;
(asdf:defsystem #:captive-neovim
  :description "Ecapsulate a NeoVIM instance with full control from Common Lisp"
  :author "StackSmith <fpgasm@apple2.x10.mx>"
  :license "BSD 3-clause license"
  :serial t
  :depends-on (:external-program
	       :cl-interpol ;; convenient...for now
	       )
  :components ((:file "package")
	       ;;(:file "global")
	       (:file "msgpack-in")
	       (:file "msgpack-out")
	       (:file "parse-tree")
	       (:file "nvim")

	       (:file "notif-redraw")
	       (:file "toy")))

