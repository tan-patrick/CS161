;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; SBCL throwing errors for setq (remove later)
(defvar blank)
(defvar wall)
(defvar box)
(defvar keeper)
(defvar star)
(defvar boxstar)
(defvar keeperstar)
(defvar p1)
(defvar p2)
(defvar p3)
(defvar p4)
(defvar p5)
(defvar p6)
(defvar p7)
(defvar p8)
(defvar p9)
(defvar p10)
(defvar p11)
(defvar p12)
(defvar p13)
(defvar p14)
(defvar p15)
(defvar p16)
(defvar p17)
(defvar p18)
(defvar p19)
(defvar p20)
(defvar p21)
(defvar p22)

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

;
; Helper function of for goal-test
; Checks if the row has a box not in a goal
; Returns nil if there is a box not in goal
(defun checkGoalCol (r)
  (cond ((null r) t)
	(t (if (isBox (car r))
	       nil
	     (checkGoalCol (cdr r))
	     );end if
	   );end t
	);end cond
  )

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
; We simply look check if each row has a box in it that is not on a goal. If it does, return nil. If we reach the end of the list, return t.
(defun goal-test (s)
  (cond ((null s) t)
	(t (if (= (count 2 (car s)) 0)
		 (goal-test (cdr s))
		 ;otherwise box not in goal was found
		 nil
		 );end if
	 );end t
	);end cond
  );end defun

; Returns integer at column x or row r
; helper function for get-square
; passes in cdr r until x = 0, which is the correct column
(defun checkSquareColumn (r x)
	(cond ((null r) 1)
		((= x 0) (car r))
		(t (checkSquareColumn (cdr r) (- x 1)))))

; Returns the integer value at the location x y in state s
; passes cdr s until the y value gets to 0, then call helper function
(defun get-square (s x y)
	(cond ((null s) 1)
		((= y 0) (checkSquareColumn (car s) x))
		(t (get-square (cdr s) x (- y 1)))))

; Sets a column to a new value
; helper function for set-square
; Changes a single square in a row and returns the list

(defun setSquareColumn (r x v)
	(cond ((null r) nil)
		((= x 0) (append (list v) (cdr r)))
		(t (append (list (car r)) (setSquareColumn (cdr r) (- x 1) v)))))

; Sets a location to a new value
; if y is not yet 0, append the first list in the state to the call of set-square on cdr s
; if y = 0, then append the changed list to the cdr s (rest of state)
; The changed list is created through the helper setSquareColumn
; note: returns original state if location is out of bounds

(defun set-square (s x y v)
	(cond ((null s) nil)
		((= y 0) (append (list (setSquareColumn (car s) x v)) (cdr s)))
		(t (append (list (car s)) (set-square (cdr s) x (- y 1) v)))))

; helper function. tries to move the keeper up and updates the state if possible
; Check the spot above the keeper. If it is a box or box on goal, then we need to check the space above the box
; If it is not a wall, we can push the box
; If the space above is empty of just a goal, we can just move the keeper onto the space
; For all moves, we also have to check what the keeper was currently standing on and set the space (nothing or a goal)
; More comments below

(defun try-move-up (s x y)
	(let ((currentsquare (get-square s x y))
		(1above (get-square s x (- y 1)))
		(2above (get-square s x (- y 2)))) ; find square values for the three squares that might switch
			(cond ((= 1above 1) nil) ; if wall above, unable to move (also checks out of bounds)
				((and (or (= 1above 2) (= 1above 5)) (not (or (= 2above 1) (= 2above 2)))) ;if the space above is a box and two spaces above is available
					(let ((movedoffcurrent (cond ((= currentsquare 3) (set-square s x y 0))
							((= currentsquare 6) (set-square s x y 4))))) ; set the spot keeper i currently at depending on what the spot currently is
							(let ((movedonto (cond ((= 1above 2) (set-square movedoffcurrent x (- y 1) 3))
									((= 1above 5) (set-square movedoffcurrent x (- y 1) 6))))) ; set the spot box is currently at depending on what the box is on
								(cond ((= 2above 0) (set-square movedonto x (- y 2) 2))
									((= 2above 4) (set-square movedonto x (- y 2) 5)))))) ; set the spot box is pushed into depending on what the square is
				(t (let ((movedkeeper (cond ((= 1above 0) (set-square s x (- y 1) 3)) ; otherwise, the space above is eitehr a goal or empty
										((= 1above 4) (set-square s x (- y 1) 6))))) ; set the spot the keeper is moving into
											(cond ((= currentsquare 3) (set-square movedkeeper x y 0)) ; set the current keeper spot depending on what the square currently is
												((= currentsquare 6) (set-square movedkeeper x y 4)))))))) ; end of let and t

; helper function. tries to move the keeper down and updates the state if possible
; see try-move-up for more information
(defun try-move-down (s x y)
	(let ((currentsquare (get-square s x y))
		(1below (get-square s x (+ y 1)))
		(2below (get-square s x (+ y 2)))) ; find square values for the three squares that might switch
			(cond ((= 1below 1) nil) ; if wall below, unable to move (also checks out of bounds)
				((and (or (= 1below 2) (= 1below 5)) (not (or (= 2below 1) (= 2below 2)))) ;if the space below is a box and two spaces below is available
					(let ((movedoffcurrent (cond ((= currentsquare 3) (set-square s x y 0))
							((= currentsquare 6) (set-square s x y 4))))) ; set the spot keeper i currently at depending on what the spot currently is
							(let ((movedonto (cond ((= 1below 2) (set-square movedoffcurrent x (+ y 1) 3))
									((= 1below 5) (set-square movedoffcurrent x (+ y 1) 6))))) ; set the spot box is currently at depending on what the box is on
								(cond ((= 2below 0) (set-square movedonto x (+ y 2) 2))
									((= 2below 4) (set-square movedonto x (+ y 2) 5)))))) ; set the spot box is pushed into depending on what the square is
				(t (let ((movedkeeper (cond ((= 1below 0) (set-square s x (+ y 1) 3)) ; otherwise, the space below is eitehr a goal or empty
										((= 1below 4) (set-square s x (+ y 1) 6))))) ; set the spot the keeper is moving into
											(cond ((= currentsquare 3) (set-square movedkeeper x y 0)) ; set the current keeper spot depending on what the square currently is
												((= currentsquare 6) (set-square movedkeeper x y 4)))))))) ; end of let and t

; helper function. tries to move the keeper left and updates the state if possible
; see try-move-up for more information
(defun try-move-left (s x y)
	(let ((currentsquare (get-square s x y))
		(1left (get-square s (- x 1) y))
		(2left (get-square s (- x 2) y))) ; find square values for the three squares that might switch
			(cond ((= 1left 1) nil) ; if wall left, unable to move (also checks out of bounds)
				((and (or (= 1left 2) (= 1left 5)) (not (or (= 2left 1) (= 2left 2)))) ;if the space left is a box and two spaces left is available
					(let ((movedoffcurrent (cond ((= currentsquare 3) (set-square s x y 0))
							((= currentsquare 6) (set-square s x y 4))))) ; set the spot keeper i currently at depending on what the spot currently is
							(let ((movedonto (cond ((= 1left 2) (set-square movedoffcurrent (- x 1) y 3))
									((= 1left 5) (set-square movedoffcurrent (- x 1) y 6))))) ; set the spot box is currently at depending on what the box is on
								(cond ((= 2left 0) (set-square movedonto (- x 2) y 2))
									((= 2left 4) (set-square movedonto (- x 2) y 5)))))) ; set the spot box is pushed into depending on what the square is
				(t (let ((movedkeeper (cond ((= 1left 0) (set-square s (- x 1) y 3)) ; otherwise, the space left is eitehr a goal or empty
										((= 1left 4) (set-square s (- x 1) y 6))))) ; set the spot the keeper is moving into
											(cond ((= currentsquare 3) (set-square movedkeeper x y 0)) ; set the current keeper spot depending on what the square currently is
												((= currentsquare 6) (set-square movedkeeper x y 4)))))))) ; end of let and t

; helper function. tries to move the keeper right and updates the state if possible
; see try-move-up for more information
(defun try-move-right (s x y)
	(let ((currentsquare (get-square s x y))
		(1right (get-square s (+ x 1) y))
		(2right (get-square s (+ x 2) y))) ; find square values for the three squares that might switch
			(cond ((= 1right 1) nil) ; if wall right, unable to move (also checks out of bounds)
				((and (or (= 1right 2) (= 1right 5)) (not (or (= 2right 1) (= 2right 2)))) ;if the space right is a box and two spaces right is available
					(let ((movedoffcurrent (cond ((= currentsquare 3) (set-square s x y 0))
							((= currentsquare 6) (set-square s x y 4))))) ; set the spot keeper i currently at depending on what the spot currently is
							(let ((movedonto (cond ((= 1right 2) (set-square movedoffcurrent (+ x 1) y 3))
									((= 1right 5) (set-square movedoffcurrent (+ x 1) y 6))))) ; set the spot box is currently at depending on what the box is on
								(cond ((= 2right 0) (set-square movedonto (+ x 2) y 2))
									((= 2right 4) (set-square movedonto (+ x 2) y 5)))))) ; set the spot box is pushed into depending on what the square is
				(t (let ((movedkeeper (cond ((= 1right 0) (set-square s (+ x 1) y 3)) ; otherwise, the space right is eitehr a goal or empty
										((= 1right 4) (set-square s (+ x 1) y 6))))) ; set the spot the keeper is moving into
											(cond ((= currentsquare 3) (set-square movedkeeper x y 0)) ; set the current keeper spot depending on what the square currently is
												((= currentsquare 6) (set-square movedkeeper x y 4)))))))) ; end of let and t

; helper function for next-states, calls one of the direction try-move functions above
; calls try-move-direction: direction depends on what value d was passed in
; Returns the new state or nil
(defun try-move (s x y d)
	(cond ((= d 0) (try-move-up s x y))
		((= d 1) (try-move-down s x y))
		((= d 2) (try-move-left s x y))
		((= d 3) (try-move-right s x y))))

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move s x y 0) (try-move s x y 1) (try-move s x y 2) (try-move s x y 3))) ; Creates a list of all states that are possible with the next move
	 )
    (cleanUpList result);end
   );end let
  );

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
	0
  )

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;
; Is this heuristic admissable?
; It is admissible because it always never overestimates. It only underestimates or gets the exact number of moves.
; We know it never overestimates because we only add 1 for each box not on a goal. At minimum, we will need at least one move per box not on the goal,
; since we can only move one box each move at most.

; counts the number of boxes in each row and adds them up recursively
(defun h1 (s)
  (cond ((null s) 0)
	(t (+ (count 2 (car s)) (h1 (cdr s))));end t
	);end cond
  );end defun

; Helper function of for find-all-v
; Returns all squares with value v in r
; Checks if the current car r is equal to v and adds the location to the list if it is
; Checks all elements in the row in case there is more than one of v in the row
(defun find-all-row (r curRow curCol v)
  (cond ((null r) nil)
	(t (if (= (car r) v)
	       (cons (list curRow curCol) (find-all-row (cdr r) (+ curRow 1) curCol v))
	     (find-all-row (cdr r) (+ curRow 1) curCol v)
	     );end if
	   );end t
	);end cond
  )

; Helper function for heuristic
; Goes through each row
; If there is at least one space with the value, call find-all-row for the given row which will return a list of all spaces with the given value
; Otherwise, just move on to the next row in the state
(defun find-all-v (s curCol v)
  (cond ((null s) nil)
	(t (append (find-all-row (car s) 0 curCol v) (find-all-v (cdr s) (+ curCol 1) v)));end t
	);end cond
)

; Helper function for find-total-min-distance
; Finds the minimum distance for a given box to any empty goal
; Finds the distance to the horizontal and vertical separately, then sums them
; Returns the smaller of the added distance and the recursive call with the rest of goals
; This is done by finding the distance and comparing it to the minimum of the rest of the goals
; The lowest one is returned
; We return 1000 when we reach the end (nothing should be further than 1000, presumably)
; We need a few if statements, since we have no absolute value function
(defun find-min-distance (x y goals)
	(if (null goals) 1000
	(let ((curx (car (car goals)))
		(cury (car (cdr (car goals))))) ; Sets goal locations (first goal in list)
		(let ((distancex (if (> x curx) (- x curx) (- curx x)))
			(distancey (if (> y cury) (- y cury) (- cury y)))) ; Subtracts the goal location with passed location (returns positive value like an abs. value function would have)
			(cond ((null (cdr goals)) (+ distancex distancey))
				(t (let ((min-rest (find-min-distance x y (cdr goals)))) ; Calls find-min-distance with the rest of the goals
					(if (> (+ distancex distancey) min-rest) min-rest (+ distancex distancey)) ; Returns minimum
				))))))
	)

; helper function for heuristic
; This will sum up the minimum distance from all boxes to empty goals
; It does this by calling find-min-distance for each box and adding the results recursively
(defun find-total-min-distance (boxes goals)
	(cond ((null boxes) 0)
		(t (+ (find-min-distance (car (car boxes)) (car (cdr (car boxes))) goals) (find-total-min-distance (cdr boxes) goals)))))

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
; My heuristic finds the total distance from each box to its closest empty goal
; Essentially, it sums up the minimum distance from each box to any empty goal
; It is admissible because there is no way to move any box to a goal faster than the minimum distance since a box can only move 1 space per turn at the most
; Therefore, it never overestimates so it is admissible.

; The main function finds all boxes and all empty goals and passes the lists into helper function total-min-distance
(defun h204158646 (s)
	(let ((boxes (cleanUpList (find-all-v s 0 2)))
		(emptygoals (cleanUpList (find-all-v s 0 4))))
		(find-total-min-distance boxes emptygoals)
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44) .842 .905
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111) 6.727 7.774
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(?? h1 4776,25) .2496
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??h1 1037,21) .078
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun