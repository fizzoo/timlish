(load "test.lisp")

(sb-ext:save-lisp-and-die "timlish.exe" :toplevel #'main :executable t)
