;;; This could be improved quite a lot.

(defvar *grammar-rules*
 '((S -> NP VP)
   (VP -> V NP)
   (VP -> VP PP)
   (NP -> NP PP)
   (NP -> NP NP)
   (PP -> P NP)))
          
(defvar *pos-rules*
  '((NP -> "people")
    (NP -> "fish")
    (NP -> "tanks")
    (NP -> "rods")
    (V -> "people")
    (V -> "fish")
    (V -> "tanks")
    (P -> "with")))

(defvar *pos-rules-2*
  '((NP -> "binoculars")
    (NP -> "bob")
    (NP -> "mary")
    (NP -> "saw")
    (V -> "saw")
    (P -> "with")))

(defvar *grammar-rules-2*
 '((S -> NP VP)
   (VP -> V NP)
   (VP -> VP PP)
   (PP -> P NP)
   (NP -> NP PP)))

;; going to return a list of (index postag word)
(defun pos-tag (words rules)
  (pos-tag-help 0 words rules rules))

(defun pos-tag-help (index words rules orig-rules)
  (cond 
    ((null words) nil)
    ;; out of rules, on to the next word!
    ((null rules) 
     (pos-tag-help (+ 1 index) (cdr words) orig-rules orig-rules))

    ;; current word matches current rule
    ((equalp (car words) (caddar rules))
     (cons (list index (caar rules) (car words))
           (pos-tag-help index words (cdr rules) orig-rules)))

    ;; current word does not match current rule, try next rule.
    (t (pos-tag-help index words (cdr rules) orig-rules))))


(defun width-start-split-triples (wordlength)
    (loop for width from 2 to wordlength append
      ;; for start in range(0, len(words) + 1 - width):
      (loop for start from 0 to (- wordlength width) append
          ;; for split in range(1, width):
            (loop for split from 1 to (- width 1) collect
                  (list width start split)))))

(defun build-chart (word-grammar rule-grammar words)
  (let ((chart (make-hash-table :test #'equalp)))
    (progn
      (let ((tagged (pos-tag words word-grammar)))
        (loop for entry in tagged do
              (progn
                (format t "~a~%" entry)
                (let ((span (list (car entry) (+ 1 (car entry)))))
                ;; the entries in the hash table are keyed by (start, end) and
                ;; they are lists of what's in that span.
                (setf (gethash span chart)
                      (cons (list (cadr entry) (caddr entry) nil)
                            (gethash span chart)))))))

      ;; now do production rules
      ;; for every width 2..whole sentence
      (loop for triple in (width-start-split-triples (length words)) do
        (let* ((width (car triple))
               (start (cadr triple))
               (split (caddr triple))
               (span (list start (+ start width))))
          ;; for every production...
          (loop for rule in rule-grammar do
                (let ((A (caddr rule))
                      (B (cadddr rule)))
                  (loop for leftcell in
                        (gethash (list start (+ start split)) chart) do
                    (loop for rightcell in
                        (gethash (list (+ start split) (+ start width)) chart) do
                        (if (and (equalp (car leftcell) A)
                                 (equalp (car rightcell) B))
                          (setf (gethash span chart)
                                (cons (list (car rule) leftcell rightcell)
                                      (gethash span chart))))))))))
      chart)))

(format t "####################################~%")
(format t "example: people fish fish~%")
(defparameter *sentence* (list "people" "fish" "fish"))
(setf parsed
  (build-chart *pos-rules* *grammar-rules* *sentence*))

(format t "contents of the chart...~%")
(loop for key being the hash-key of parsed
      do (progn
           (format t "~a~%" key)
           (format t "  ~a~%" (gethash key parsed))))

(loop for entry in (gethash (list 0 (length *sentence*)) parsed) do
      (if (equalp (car entry) 'S)
        (format t "complete parse: ~a~%" entry)))


(format t "####################################~%")
(format t "example: mary saw bob with binoculars~%")
(defparameter *sentence-2* (list "mary" "saw" "bob" "with" "binoculars"))

(setf parsed
  (build-chart *pos-rules-2* *grammar-rules-2* *sentence-2*))

(format t "contents of the chart...~%")
(loop for key being the hash-key of parsed
      do (progn
           (format t "~a~%" key)
           (format t "  ~a~%" (gethash key parsed))))

(loop for entry in (gethash (list 0 (length *sentence-2*)) parsed) do
      (if (equalp (car entry) 'S)
        (format t "complete parse: ~a~%" entry)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WIP; make this much cleaner and lispier. The earlier version is a close
;; transliteration of some Python, and it's pretty gross in Common Lisp.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse (words)
  (let* ((tagged (pos-tag words *pos-rules*)))
    (parse-help tagged *grammar-rules*)))

(defun parse-help (inventory rules)
  ;; apply rules until we make a pass over the rules and achieve quiescence
  ;; so if we go over all the rules and nothing new fired, then we're done.
  ;; at each step, we return the new inventory.

  (let ((next-inventory (parse-help-one-pass inventory rules)))
    (cond
      ((equalp next-inventory inventory) next-inventory)
      (t (parse-help next-inventory rules)))))

(defun parse-help-one-pass (inventory rules)
  (cond
    ;; if we're out of rules, we're done.
    ((null rules) inventory)

    ;; check to see if we can apply the first rule. if we can, apply it and go
    ;; to the next rule.
    ((rule-applies (car rules) (inventory))
     (parse-help-one-pass 
      (cons (apply-rule (car rules) inventory) inventory)
      (cdr rules)))

    ;; if the first rule doesn't apply, go to the next one.
    (t (parse-help-one-pass inventory (cdr rules)))))
