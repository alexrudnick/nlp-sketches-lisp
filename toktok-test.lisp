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


;; XXX: add a test to show that we handle ] correctly.

(test dr-bob-test
  (is (equalp *dr-bob-correct*
              (tokenize::toktok-tokenize *dr-bob-joined*))))


;;     >>> toktok = ToktokTokenizer()
;;     >>> text = u'Is 9.5 or 525,600 my favorite number?'
;;     >>> print(toktok.tokenize(text, return_str=True))
;;     Is 9.5 or 525,600 my favorite number ?
;;     >>> text = u'The https://github.com/jonsafari/tok-tok/blob/master/tok-tok.pl is a website with/and/or slashes and sort of weird : things'
;;     >>> print(toktok.tokenize(text, return_str=True))
;;     The https://github.com/jonsafari/tok-tok/blob/master/tok-tok.pl is a website with/and/or slashes and sort of weird : things


;;    >>> text = u'\xa1This, is a sentence with weird\xbb symbols\u2026 appearing everywhere\xbf'
;;    >>> expected = u'\xa1 This , is a sentence with weird \xbb symbols \u2026 appearing everywhere \xbf'
;;    >>> assert toktok.tokenize(text, return_str=True) == expected
;;    >>> toktok.tokenize(text) == [u'\xa1', u'This', u',', u'is', u'a', u'sentence', u'with', u'weird', u'\xbb', u'symbols', u'\u2026', u'appearing', u'everywhere', u'\xbf']
;;    True

;; Don't tokenize period unless it ends the line and that it isn't
;; preceded by another period, e.g.
;; "something ..." -> "something ..."
(test eol-periods-test
  (is (equalp (tokenize::split-by-spaces "something ...")
              (tokenize::toktok-tokenize "something ..."))))

;; "something." -> "something ."
(test eol-single-period-test
  (is (equalp (tokenize::split-by-spaces "something .")
              (tokenize::toktok-tokenize "something."))))

;; Don't tokenize period unless it ends the line eg.
;; " ... stuff." ->  "... stuff ."
(test beginning-periods-test
  (is (equalp (tokenize::split-by-spaces "... stuff .")
              (tokenize::toktok-tokenize "... stuff."))))

(test internal-ellipsis-test
  (is (equalp (tokenize::split-by-spaces "also more ... stuff .")
              (tokenize::toktok-tokenize "also more ... stuff."))))

;; Demonstrate that the "FUNKY_PUNCT_1" and "FUNKY_PUNCT_2" patterns do what
;; they're supposed to do. For example, FUNKY_PUNCT_1 splits out inverted question
;; marks.
(test funky-punct-1-test
  (is (equalp (tokenize::split-by-spaces "¿ Quieres una taza de café ?")
              (tokenize::toktok-tokenize "¿Quieres una taza de café?"))))

;; This one would have failed without the FUNKY_PUNCT_2 pattern included.
(test funky-punct-2-test
  (is (equalp (tokenize::split-by-spaces "« Sí , por favor . »")
              (tokenize::toktok-tokenize "«Sí, por favor.»"))))

(run)
