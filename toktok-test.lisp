(load "~/quicklisp/setup.lisp")

;; is there a cleaner way to include this?
(load "tokenize.lisp")

(ql:quickload "1am")


(defpackage :tokenize-test (:use :cl :1am :tokenize))
(in-package :tokenize-test)



(defconstant *dr-bob-joined*
    "They thought, \"Is 9.5 or 525,600 my favorite number?\"  before seeing Dr. Bob's dog talk.")

(defconstant *dr-bob-correct*
  (tokenize::split-by-spaces
    "They thought , \" Is 9.5 or 525,600 my favorite number ? \" before seeing Dr. Bob ' s dog talk ."))



(tokenize::print-word-list
  (tokenize::toktok-tokenize
    *dr-bob-joined*))

(test split-by-spaces-test
  (is (equalp *dr-bob-correct*
              (tokenize::toktok-tokenize *dr-bob-joined*))))

(run)
