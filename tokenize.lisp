;; Say we wanted to write a nice tokenizer in Common Lisp.

;; The real goal will be to do something at least as nice as the Punkt
;; tokenizer in NLTK, and also the sent_tokenize function.

;; open question: What's the situation with Unicode in Common Lisp?

;; first idea, though, can we just take a string and return a list of strings?
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

(defun whitespacep (c)
  (cond
    ((equalp #\Space c) t)
    ((equalp #\Newline c) t)
    (t nil)))

(defun print-word-list (words)
  (progn
    (format t "here's you some words...~%")
    (loop for word in words do
          (format t "word: [~a] ~%" word))))
