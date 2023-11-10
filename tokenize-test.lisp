(load "~/quicklisp/setup.lisp")

;; is there a cleaner way to include this?
(load "tokenize.lisp")

(ql:quickload "1am")


(defpackage :tokenize-test (:use :cl :1am :tokenize))
(in-package :tokenize-test)

(defconstant *properly-split*
  (list "this" "ought" "to" "return" "a" "list" "of" "words"))

(test split-by-spaces-test
  (is (equalp *properly-split*
              (tokenize::split-by-spaces
                "this ought to return a list of words")))
  (is (equalp *properly-split*
              (tokenize::split-by-spaces
                "this       ought to return a list of words")))
  (is (equalp *properly-split*
              (tokenize::split-by-spaces
                "this ought to return a list of words     ")))
  (is (equalp *properly-split*
              (tokenize::split-by-spaces
                "  this ought to return a list of words     "))))

(run)
