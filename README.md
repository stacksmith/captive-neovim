# CAPTIVE-NEOVIM

## Summary:

A minimalistic wrapper around a captured NeoVIM process, providing a _real NeoVIM_ experience inside Common Lisp applications.

The provided nvim class takes care of RPC with NeoVIM.  Keystrokes may be sent to NeoVIM, while generic functions are invoked to service NeoVIM's screen upates.  A toy demo demonstrates an in-repl control of a NeoVIM instance. 

Resource utilization and dependencies are minimal: the project requires no multithreading, and provides its own state-machine messagepack decoder to minimize consing.

Status: proof of concept / early development.

## Requirements

* A functioning NeoVIM installation;
* external-program (to start NeoVIM portably)

Optionally,

* cl-interpol - to simplify sending escape and complicated key sequences for now.

## Hands-on

A working 'vimtoy' implementation provides a simple demo of the wrapper.

```
(ql:quickload :captive-neovim)
(in-package :cn)
CN> (init)
CN> (connect (make-instance 'vimtoy)  32 10) ;; create a 32 x 10 neovim
CN> (updates *v*)
CN> ...
...
================================
~                               
~                               
~                               
~                               
~                               
~                               
~                               
[No Name]       0,0-1        All

================================
                                
CN> (! "iHello World")
CN> (updates *v*)
================================
Hello World                     
~                               
~                               
~                               
~                               
~                               
~                               
[No Name] [+]   1,12         All
-- INSERT --                    
================================

CN> (ql:quickload :cl-interpol)
CN> (interpol:enable-interpol-syntax)
CN> (! #?"\e")
```

Note: some screen noise omitted for brevity.

## STATUS

Early development.



	



