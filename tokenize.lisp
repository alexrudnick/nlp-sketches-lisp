;; Say we wanted to write a nice tokenizer in Common Lisp.

;; The real goal will be to do something at least as nice as the Punkt
;; tokenizer in NLTK, and also the sent_tokenize function.

;; open question: What's the situation with Unicode in Common Lisp?

(load "~/quicklisp/setup.lisp")
(ql:quickload "cl-ppcre")
 
(defpackage :tokenize (:use :cl :cl-ppcre))
(in-package :tokenize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; split-by-spaces: simplest tokenization situation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun split-by-spaces (text)
  (mapcar (lambda (backwards-letters)
            (coerce (nreverse backwards-letters) 'string))
    (split-by-spaces-list '() (coerce text 'list))))

(defun split-by-spaces-list (cur-word characters-left)
  ;; this ought to return a list of strings.
  (cond 
    ((null characters-left)
     (if cur-word
       (list cur-word)
       '()))
    ((whitespacep (car characters-left))
     (if cur-word
       (cons cur-word (split-by-spaces-list '() (cdr characters-left)))
       (split-by-spaces-list '() (cdr characters-left))))

    (t (split-by-spaces-list
         (cons (car characters-left) cur-word)
         (cdr characters-left)))))

;; XXX: we could have a more expansive version of whitespace detection, and
;; there may be a good predicate for this in the standard library.
(defun whitespacep (c)
  (cond
    ((equalp #\Space c) t)
    ((equalp #\Newline c) t)
    (t nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; toktok from NLTK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; this is going to use the regex-replace-all function in cl-ppcre
(defun toktok-tokenize (text)
  (split-by-spaces
    (apply-every-regex *toktok-regex-pairs* text)))

(defun apply-every-regex (regex-pairs text)
  (cond
    ((null regex-pairs) text)
    (t (let ((regex (caar regex-pairs))
             (replacement (cdar regex-pairs)))
         (apply-every-regex
           (cdr regex-pairs)
           (progn
             (let ((next-text (regex-replace-all regex text replacement)))
               (format t "~a~%" next-text)
               next-text)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; misc utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-word-list (words)
  (progn
    (format t "here's you some words...~%")
    (loop for word in words do
          (format t "word: [~a] ~%" word))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; toktok patterns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "toktok-patterns.lisp")
