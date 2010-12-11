This is ERT, a tool for automated testing in Emacs Lisp.

In preparation for inclusion into Emacs, this repository is structured
to match the layout of Emacs' source tree.

If you want to use ERT, all you need are the .el files in
lisp/emacs-lisp; put them somewhere in your load-path and compile
them.  You can also run ert's self-tests by compiling & loading the
.el files in test/automated and typing M-x ert RET RET.

ERT's documentation is in doc/misc/ert.texi.
