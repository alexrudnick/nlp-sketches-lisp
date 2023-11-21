(load "~/quicklisp/setup.lisp")

;; is there a cleaner way to include this?
(load "tokenize.lisp")

(defpackage :tokenize-demo (:use :cl :tokenize))
(in-package :tokenize-demo)

(loop for line = (read-line *standard-input* nil nil)
      while line
      do (progn
           (tokenize::print-word-list (tokenize::toktok-tokenize line))))
