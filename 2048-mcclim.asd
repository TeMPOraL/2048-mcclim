;;;; chronoton.asd

(asdf:defsystem #:2048-mcclim
  :serial t

  :long-name "2048 Game - McCLIM version."
  :author "Jacek Złydach"
  :version (:read-file-from "version.lisp" :at (1 2 2))
  :description "TODO."

  :license "MIT"
  :homepage "https://github.com/TeMPOraL/2048-mcclim"
  :bug-tracker "https://github.com/TeMPOraL/2048-mcclim/issues"
  :source-control (:git "https://github.com/TeMPOraL/2048-mcclim.git")
  :mailto "temporal.pl+2048mcclim@gmail.com"

  :encoding :utf-8
  
  :depends-on (#:alexandria
               #:mcclim)
  
  :components ((:file "package")
               (:file "version")

               (:file "main")))

