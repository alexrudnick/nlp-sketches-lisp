(load "~/quicklisp/setup.lisp")

(ql:quickload "1am")


(defpackage :tokenize-test (:use :cl :1am))
(in-package :tokenize-test)

;; is there a cleaner way to include this?
(load "tokenize.lisp")

(defconstant *properly-split*
  (list "this" "ought" "to" "return" "a" "list" "of" "words"))

(test split-by-spaces-test
  (is (equalp *properly-split*
              (split-by-spaces "this ought to return a list of words")))
  (is (equalp *properly-split*
              (split-by-spaces "this       ought to return a list of words")))
  (is (equalp *properly-split*
              (split-by-spaces "this ought to return a list of words     ")))
  (is (equalp *properly-split*
              (split-by-spaces "  this ought to return a list of words     "))))

(run)
