;; Game Of Life in Common Lisp
;; Isak Falk & Alex Rudnick, pair programming!
;; Very slightly cleaned up by Alex Rudnick after the fact.

;; Common Lisp has multidimensional arrays.
(defun create-board (size)
  (make-array (list size size)))

(defun create-oscillator-board ()
  (let ((output (create-board 5)))
    (progn
      (setf (aref output 1 1) T)
      (setf (aref output 1 2) T)
      (setf (aref output 1 3) T)
    output)))

;; what are the rules exactly?

;; for each cell
;; -- check every neighbor and count how many are truthy
;; -- based on how many are truthy, that gives you the status for next time.

;; we've decided: if you reach off the edge of the board, then that counts as
;; dead.

;; Relative offsets for all the neighbors of a cell.
(defconstant neighbors-offsets
      '( (-1 -1)
         (-1 0)
         (-1 1)
         (0 -1)
         (0 1)
         (1 -1)
         (1 0)
         (1 1)))

(defun check-alive (board i j)
  (cond
    ((< i 0) 0)
    ((< j 0) 0)
    ((>= i (cadr (array-dimensions board))) 0)
    ((>= j (car (array-dimensions board))) 0)
    (t (if (aref board i j) 1 0))))

;; count-alive-neighbors
(defun count-alive-neighbors (board i j)
  ;; diagonals count as neighbors
  (loop for offset in neighbors-offsets
        sum (check-alive board 
                       (+ i (car offset))
                       (+ j (cadr offset))) into total
        finally (return total)))

;; next-board
;; return a completely fresh board that contains the next state
;; any live cell with 2 or 3 neighbors survives
;; if the cell is false, and it has 3 live neighbors, it becomes alive
;; any other cell becomes false.

(defun make-indices (boardsize)
  (loop for i from 0 to (- boardsize 1) append
        (loop for j from 0 to (- boardsize 1) collect (list i j))))

(defun next-board (board)
  (let* ((boardsize (car (array-dimensions board)))
         (output (create-board boardsize))
         (indices (make-indices boardsize)))

         (loop for idx in indices do
               (let* ((i (car idx))
                      (j (cadr idx))
                      (num-alive (count-alive-neighbors board i j))
                      (is-alive (aref board i j))) 
                 (cond
                   ((and is-alive (find num-alive (list 2 3)))
                    (setf (aref output i j) T))
                   ((and (not is-alive) (= num-alive 3))
                    (setf (aref output i j) T))
                   (T (setf (aref output i j) nil)))))
         output))
