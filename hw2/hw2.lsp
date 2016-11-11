; DFS will take any TREE and output the order that a DFS search would visit nodes.
; TREE is any tree
; output will be a list of nodes in the order of a DFS search

; We will traverse down the first branch until we hit null. Then, we move on to the next branch
; This imitates the DFS order. We print out a node element whenever we reach one.

(defun DFS (TREE)
	(cond ((null TREE) nil)
		((atom TREE) (list TREE))
		(t (APPEND (DFS (first tree)) (DFS (rest tree))))))

; PRINT-LEVEL is a helepr function for DFID
; It takes any input TREE and outputs all nodes at the top level of the tree.

(defun PRINT-LEVEL (TREE)
	(cond ((null TREE) nil)
		((atom (first TREE)) (APPEND (list (first TREE)) (PRINT-LEVEL (rest tree))))
		(t (PRINT-LEVEL (rest tree)))))

; TO-ITERATIVE-DEPTH is a helper function
; It will take a TREE and some DEPTH and print out all nodes up to the given depth in DFS order.		

(defun TO-ITERATIVE-DEPTH (TREE DEPTH)
	(cond ((null TREE) nil)
		((atom TREE) (list TREE))
		((equal 1 DEPTH) (PRINT-LEVEL TREE))
		(t (APPEND (TO-ITERATIVE-DEPTH (first tree) (- DEPTH 1)) (TO-ITERATIVE-DEPTH (rest tree) DEPTH)))))

; DFID-HELPER is a helper function
; It will run TO-ITERATIVE DEPTH from 1 to the maximum depth input by DFID

(defun DFID-HELPER (TREE DEPTH MAX)
	(cond ((equal MAX 0) nil)
		((< MAX DEPTH) nil)
		(t (append (TO-ITERATIVE-DEPTH TREE DEPTH) (DFID-HELPER TREE (+ DEPTH 1) MAX)))))

; DFID runs a iterative DFS using helper functions above

(defun DFID (TREE DEPTH)
	(DFID-HELPER TREE 1 DEPTH))

; These functions implement a depth-first solver for the missionary-cannibal
; problem. In this problem, three missionaries and three cannibals are trying to
; go from the east side of a river to the west side. They have a single boat
; that can carry two people at a time from one side of the river to the
; other. There must be at least one person in the boat to cross the river. There
; can never be more cannibals on one side of the river than missionaries. If
; there are, the cannibals eat the missionaries.

; In this implementation, a state is represented by a single list
; (missionaries cannibals side). side represents which side the boat is
; currently on, and is T if it is on the east side and NIL if on the west
; side. missionaries and cannibals represent the number of missionaries and
; cannibals on the same side as the boat. Thus, the initial state for this
; problem is (3 3 T) (three missionaries, three cannibals, and the boat are all
; on the east side of the river) and the goal state is (3 3 NIL).

; The main entry point for this solver is the function MC-DFS, which is called
; with the initial state to search from and the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, MC-DFS returns NIL.

; To call MC-DFS to solve the original problem, one would call (MC-DFS '(3 3 T)
; NIL) -- however, it would be possible to call MC-DFS with a different initial
; state or with an initial path.

; Examples of calls to some of the helper functions can be found after the code.



; FINAL-STATE takes a single argument s, the current state, and returns T if it
; is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
  (cond ((EQUAL '(3 3 NIL) s) t)
  	(t NIL)))

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (s), a number of
; missionaries to move (m), and a number of cannibals to move (c). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).
(defun next-state (s m c)
  (let ((cur_missionaries (- (first s) m))
  	(cur_cannibals (- (second s) c))
  	(cur_side (third s)))
  		(let ((new_missionaries (- 3 cur_missionaries)) ; the number of missionaries and cannibals on the other side will be 3 - the number on the current side
  			(new_cannibals (- 3 cur_cannibals))
  			(new_side (not cur_side)))
		  		(cond ((< cur_missionaries 0) NIL) ; check if there are valid amounts of people or if a side has more missionaries than cannibals, otherwise output the list as valid
		  			((< cur_cannibals 0) NIL)
		  			((AND (> cur_cannibals cur_missionaries) (> cur_missionaries 0)) NIL)
		  			((AND (> new_cannibals new_missionaries) (> new_missionaries 0)) NIL)
		  			(t (list (list new_missionaries new_cannibals new_side)))))))

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun succ-fn (s)
  (APPEND (next-state s 0 1) (next-state s 1 0) (next-state s 1 1) (next-state s 0 2) (next-state s 2 0))) ; runs next-state for all valid combinations of missionaries and cannibals

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (s) and the
; stack of states visited by MC-DFS (states). It returns T if s is a member of
; states and NIL otherwise.
(defun on-path (s states)
  (cond ((null states) nil)
	((equal s (first states)) T)
	(t (on-path s (rest states)))))

; MULT-DFS is a helper function for MC-DFS. It takes two arguments: a stack of
; states from the initial state to the current state (path), and the legal
; successor states to the last state on that stack (states). states is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of states in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun mult-dfs (states path)
	(cond ((null (first states)) nil) ; if we reach the end, returns nil
		((final-state (first states)) (append path (list (first states)))) 
		(t (let ((dfs-searched (mc-dfs (first states) path))) ; run mc-dfs on the first state in list
			(cond ((not (null dfs-searched)) dfs-searched) ; if it finds the answer (not null), return it
				(t (mult-dfs (rest states) path))))))) ; otherwise, run mult-dfs for the next state

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun mc-dfs (s path)
  (cond ((final-state s) (append path (list s)))
  	((on-path s path) nil) ; if it is already in the path, return null. otherwise, we'll get an infinite loop
  	(t (mult-dfs (succ-fn s) (append path (list s))))))

; Function execution examples

; Applying this operator would result in an invalid state, with more cannibals
; than missionaries on the east side of the river.
; (next-state '(3 3 t) 1 0) -> NIL

; Applying this operator would result in one cannibal and zero missionaries on
; the west side of the river, which is a legal operator. (NOTE that next-state
; returns a LIST of successor states, even when there is only one successor)
; (next-state '(3 3 t) 0 1) -> ((0 1 NIL))

; succ-fn returns all of the legal states that can result from applying
; operators to the current state.
; (succ-fn '(3 3 t)) -> ((0 1 NIL) (1 1 NIL) (0 2 NIL))
; (succ-fn '(1 1 t)) -> ((3 2 NIL) (3 3 NIL))
