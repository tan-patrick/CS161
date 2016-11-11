; Patrick Tan
; Homework 4, CS 161

; The first part of the code is checking if a given state violates a constraint and if it is the final state
; The second part runs the dfs search using the constraint-violation checks that the first part implemented

; next 3 functions check the row and diagonals of a single queen against the rest of the board
; helper functions for check-single-queen
; return t if violation found

; checks the horizontal
(defun check-row (queen board)
	(cond ((> (count queen board) 0) t)
		(t nil)))

; checks the upwards diagonal
(defun check-up-right (queen board)
	(cond ((= queen 0) nil)
		((null board) nil)
		((= (- queen 1) (first board)) t)
		(t (check-up-right (- queen 1) (rest board)))))

; checks the downwards diagonal
(defun check-down-right (queen board)
	(cond ((= queen 0) nil)
		((null board) nil)
		((= (+ queen 1) (first board)) t)
		(t (check-down-right (+ queen 1) (rest board)))))

; takes a queen location and the rest of the board (right of the queen) and check if there is a violation
; return t if no violation

(defun check-single-queen (queen board)
	(cond ((check-row queen board) nil)
		((check-up-right queen board) nil)
		((check-down-right queen board) nil)
		(t t)))

; checks any board for a violoation
; returns t if no violation

(defun check-constraint (board)
	(cond ((null board) t)
		((check-single-queen (first board) (rest board)) (check-constraint (rest board)))
		(t nil))) ; if it reaches here, then a violation was found

; checks if a given board is a solution to the n-queens problem
; uses check-constraint for each queen with the rest of the board

(defun check-solution (board n)
	(cond ((not (= (length board) n)) nil)
		((check-constraint board) t)
		(t nil))) ; return nil if a violation found

; THIS IS OUR CONSTRAINT SATISFACTION FUNCTION OF THE SOLUTION: it checks if the new state is valid or if there is a violation 
; tries adding a new value in the next column
; returns t if there is no violation (valid)
; When there is a violation of the constraint, we stop the dfs search and try the next board-state

(defun try-value-in-column (board row-value)
	(cond ((check-constraint (append board (list row-value))) t)
		(t nil))) ; return nil if violation


; tests all values for the next column
; returns a list of lists of all possible states for the next column given a current board that do not have any violations

(defun try-column (board current n)
	(cond ((> current n) nil)
		((try-value-in-column board current) (cons (append board (list current)) (try-column board (+ current 1) n)))
		(t (try-column board (+ current 1) n))))

; runs a dfs for n-queens
; given the initial board-states (created by create-base-states), it takes the first one from the list and runs try-column, adding whatever new states there are to the front of the list to search
; This forces the algorithm to search the last states added first (implementing to dfs part of the search)
; It searches all states, returning nil when there are no more possible states to search or the solution if one was found

(defun run-dfs (board-states n)
	(cond ((null board-states) nil) ; all possible boards searched, return nil
		((check-solution (first board-states) n) (first board-states)) ; check if the first state is a valid solution to the problem 
		(t (run-dfs (append (try-column (first board-states) 1 n) (rest board-states)) n))))

; creates a list of states where each state has the queen in one of the first columns
; i.e. for a N-Queens problem of 4, it will return ((1) (2) (3) (4))

(defun create-base-states (current n)
	(cond ((> current n) nil)
		(t (cons (list current) (create-base-states (+ current 1) n)))))

; Calls helper function run-dfs with the base states (first column) as the initial states
(defun QUEENS (n)
	(run-dfs (create-base-states 1 n) n))