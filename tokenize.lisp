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


;; All of our patterns & replacements.

(defconstant *ampercent*
  '("& "
     . "&amp; "))

(defconstant *close-punct-re*
  '("([)]}\U+F3B\U+F3D\U+169C\U+2046\U+207E\U+208E\U+232A\U+2769\U+276B\U+276D\U+276F\U+2771\U+2773\U+2775\U+27C6\U+27E7\U+27E9\U+27EB\U+27ED\U+27EF\U+2984\U+2986\U+2988\U+298A\U+298C\U+298E\U+2990\U+2992\U+2994\U+2996\U+2998\U+29D9\U+29DB\U+29FD\U+2E23\U+2E25\U+2E27\U+2E29\U+3009\U+300B\U+300D\U+300F\U+3011\U+3015\U+3017\U+3019\U+301B\U+301E\U+301F\U+FD3F\U+FE18\U+FE36\U+FE38\U+FE3A\U+FE3C\U+FE3E\U+FE40\U+FE42\U+FE44\U+FE48\U+FE5A\U+FE5C\U+FE5E\U+FF09\U+FF3D\U+FF5D\U+FF60\U+FF63])"
     . "\1 "))

(defconstant *comma-in-num*
  '("(?<!,)([,\U+60C])(?![,\d])"
     . " \1 "))

(defconstant *currency-sym-re*
  '("([$\U+A2\U+A3\U+A4\U+A5\U+58F\U+60B\U+9F2\U+9F3\U+9FB\U+AF1\U+BF9\U+E3F\U+17DB\U+20A0\U+20A1\U+20A2\U+20A3\U+20A4\U+20A5\U+20A6\U+20A7\U+20A8\U+20A9\U+20AA\U+20AB\U+20AC\U+20AD\U+20AE\U+20AF\U+20B0\U+20B1\U+20B2\U+20B3\U+20B4\U+20B5\U+20B6\U+20B7\U+20B8\U+20B9\U+20BA\U+A838\U+FDFC\U+FE69\U+FF04\U+FFE0\U+FFE1\U+FFE5\U+FFE6])"
     . "\1 "))

(defconstant *en-em-dashes*
  '("([\U+2013\U+2014])"
     . " \1 "))

(defconstant *final-period-1*
  '("(?<!\.)\.$"
     . " ."))

;; (defconstant *final-period-2*
;;   '("(?<!\.)\.\s*(["'\U+2019\U+BB\U+203A\U+201D]) *$"
;;      . " . \1"))

;; (defconstant *funky-punct-1*
;;   '("([\U+60C\U+3B\U+61B\U+BF!"\])}\U+BB\U+203A\U+201D\U+61F\U+A1%\U+66A\U+B0\U+B1\U+A9\U+AE\U+964\U+965\U+2026])"
;;      . " \1 "))

(defconstant *funky-punct-2*
  '("([({\[\U+201C\U+2018\U+201E\U+201A\U+AB\U+2039\U+300C\U+300E])"
     . " \1 "))

(defconstant *multi-commas*
  '("(,{2,})"
     . " \1 "))

(defconstant *multi-dashes*
  '("(-{2,})"
     . " \1 "))

(defconstant *multi-dots*
  '("(\.{2,})"
     . " \1 "))

(defconstant *non-breaking*
  '("\U+A0"
     . " "))

(defconstant *one-space*
  '(" {2,}"
     . " "))

(defconstant *open-punct-re*
  '("([([{\U+F3A\U+F3C\U+169B\U+201A\U+201E\U+2045\U+207D\U+208D\U+2329\U+2768\U+276A\U+276C\U+276E\U+2770\U+2772\U+2774\U+27C5\U+27E6\U+27E8\U+27EA\U+27EC\U+27EE\U+2983\U+2985\U+2987\U+2989\U+298B\U+298D\U+298F\U+2991\U+2993\U+2995\U+2997\U+29D8\U+29DA\U+29FC\U+2E22\U+2E24\U+2E26\U+2E28\U+3008\U+300A\U+300C\U+300E\U+3010\U+3014\U+3016\U+3018\U+301A\U+301D\U+FD3E\U+FE17\U+FE35\U+FE37\U+FE39\U+FE3B\U+FE3D\U+FE3F\U+FE41\U+FE43\U+FE47\U+FE59\U+FE5B\U+FE5D\U+FF08\U+FF3B\U+FF5B\U+FF5F\U+FF62])"
     . "\1 "))

(defconstant *pipe*
  '("\|"
     . " &#124\U+3B "))

(defconstant *prob-single-quotes*
  '("(['\U+2019`])"
     . " \1 "))

(defconstant *stupid-quotes-1*
  '(" ` ` "
     . " `` "))

(defconstant *stupid-quotes-2*
  '(" ' ' "
     . " '' "))

(defconstant *tab*
  '("	"
     . " &#9; "))

(defconstant *url-foe-1*
  '(":(?!//)"
     . " : "))

(defconstant *url-foe-2*
  '("\?(?!\S)"
     . " ? "))

(defconstant *url-foe-3*
  '("(:\/\/)[\S+\.\S+\/\S+][\/]"
     . " / "))

(defconstant *url-foe-4*
  '(" /"
     . " / "))


(defconstant *toktok-regex-pairs* (list *ampercent* *close-punct-re* *comma-in-num* *currency-sym-re* *en-em-dashes* *final-period-1* *funky-punct-2* *multi-commas* *multi-dashes* *multi-dots* *non-breaking* *one-space* *open-punct-re* *pipe* *prob-single-quotes* *stupid-quotes-1* *stupid-quotes-2* *tab* *url-foe-1* *url-foe-2* *url-foe-3* *url-foe-4*))

;; (defconstant *toktok-regex-pairs*
;;   '(
;;     ;; NON_BREAKING
;;     ("\u00A0" . " ")
;;     ;; FUNKY_PUNCT_1
;; 
;;     ;; URL_FOE_1
;;     ;; URL_FOE_2
;;     ;; URL_FOE_3
;;     ;; URL_FOE_4
;;     ;; AMPERCENT
;;     ;; TAB
;;     ;; PIPE
;;     ;; OPEN_PUNCT_RE
;;     ;; CLOSE_PUNCT_RE
;;     ;; MULTI_COMMAS
;;     ;; COMMA_IN_NUM
;;     ;; FINAL_PERIOD_2
;;     ("(?<!\.)\.\s*([\"'’»›”]) *$" . " . \1")
;; 
;;     ;; PROB_SINGLE_QUOTES
;;     ;; STUPID_QUOTES_1
;;     ;; STUPID_QUOTES_2
;;     ;; CURRENCY_SYM_RE
;;     ;; EN_EM_DASHES
;;     ("([–—])" . " \1 ")
;; 
;;     ;; MULTI_DASHES
;;     ;; MULTI_DOTS
;;     ;; FINAL_PERIOD_1
;;     ("(?<!\.)\.$" . " .")
;; 
;;     ;; FINAL_PERIOD_2 -- this one happens twice in the Python...
;;     ("(?<!\.)\.\s*([\"'’»›”]) *$" . " . \1")
;; 
;;     ;; ONE_SPACE
;;     (" {2,}" . " ")
;;    ))

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
           (regex-replace-all regex text replacement))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; misc utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-word-list (words)
  (progn
    (format t "here's you some words...~%")
    (loop for word in words do
          (format t "word: [~a] ~%" word))))
