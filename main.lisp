;;; -*- Mode: Lisp -*-
;;; Entry point for `sbcl --script main.lisp ...` (see ./worldtool wrapper).

(let ((here (make-pathname :name nil :type nil :defaults *load-truename*)))
  (dolist (f '("src/package" "src/constants" "src/image" "src/ilod"
               "src/vlod" "src/dump" "src/inspect" "src/symbols" "src/vbin"
               "src/emit" "src/wdecode" "src/wfun" "src/refdata"
               "src/cold-model" "src/cold-wired" "src/cold-object"
               "src/cold-fun" "src/cold-gen" "src/cold-eval"
               "src/cold-machinery" "src/cold-pkg" "src/cold-diff"
               "src/cli"))
    (load (merge-pathnames (concatenate 'string f ".lisp") here))))

(sb-ext:exit :code (worldtool:main (rest sb-ext:*posix-argv*)))
