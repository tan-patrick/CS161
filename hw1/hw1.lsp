;;; TREE-CONTAINS will check if a value appears in an ordered tree (L m R)
;;; N is the value we are searching for, TREE is the input ordered tree in which we are looking for N.
;;; We return true if the value N is in TREE and NIL if is it not.

;;; If it is an atom or just a number, that means we are at the leaf, which is the end of the search.
;;; Otherwise, we check if N is equal to the current m in (L m R). 
;;; If not, we call TREE-CONTAINS on either L or R. Because it is an ordered tree, we choose L or R depending on if N is greater than or less than m. 

(defun TREE-CONTAINS (N TREE)
	(cond ((atom TREE) (equal TREE N))
		((equal (second TREE) N) t)
		((> (second TREE) N) (TREE-CONTAINS N (first TREE)))
		(t (TREE-CONTAINS N (third TREE)))))

;;; TREE-MAX will find the maximum value in an ordered tree
;;; TREE is the input ordered tree.
;;; We will return the maximum value in the ordered tree.

;;; Since we know an ordered tree always has the higher values on the right, we essentially just travel down the right branches until reaching a leaf.
;;; If it is a list, then we pass in the right node, which is higher than the current node. If it is a number, then we reached the highest valued leaf.

(defun TREE-MAX (TREE)
	(cond ((atom TREE) TREE)
		(t (TREE-MAX (third TREE)))))

;;; TREE-ORDER will output an ordered tree and return an ordered list of values in the ordered tree.
;;; TREE is the ordered tree that we are creating the list from.
;;; We return a list of the values appearing in the ordered tree TREE.

;;; For each tree node, we will recursively call TREE-ORDER on the left and right nodes. 
;;; Since we know that all of the left hand nodes will be smaller than the number m in (L m R) and vice-versa for the right hand nodes, we append the lists returned through the recursive call in the order first (L), second (m), third (R).
;;; Our base case is a leaf, which we check if TREE is an atom. We simply return a list with only one value in it (the leaf).

(defun TREE-ORDER (TREE)
	(cond ((atom TREE) (list TREE))
		(t (APPEND (TREE-ORDER (first TREE)) (list(second TREE)) (TREE-ORDER (third TREE))) )))

;;; SUB-LIST will take a list and return a sub-list based on the input values START and LEN.
;;; L is the list we are creating the sub-list from, START is the starting index, or position, and LEN is the length of the sub-list.
;;; We return a list of values that we create from the list L.

;;; We move to the starting point by checking if START is greater than 0. If it is, we call SUB-LIST on the rest of L, or L minus the first value, and START - 1. In this way, we just "delete" items off the list until we reach the correct starting position
;;; Then, we create the sub-list by CONS the current head of L with a recursive call of the rest of L and LEN - 1.
;;; Our base case is when LEN reaches 0, which means we are done.

(defun SUB-LIST (L START LEN)
	(cond ((= LEN 0) NIL)
		((> START 0) (SUB-LIST (rest L) (- START 1) LEN))
		(t (CONS (first L) (SUB-LIST (rest L) START (- LEN 1))))))

;;; SPLIT-LIST splits a list into two halves.
;;; L is the list that we are splitting into half.
;;; We return a list that contains two lists. The two lists are each half of L. If L is odd, the second list should be one larger than the first list.

;;; We use SUB-LIST as a helper function.
;;; We get the length of the list L. Then, depending on if it is odd or even, we will pass in different values to our SUB-LIST calls.
;;; If it is even, we can just use length L / 2 for the inputs. If it is odd, after dividing length L by 2, we have to add or subtract .5 to force the second list to have the extra element.

(defun SPLIT-LIST (L)
	(let ((listLength (length L)))
		(let ((halfLength (/ listLength 2)))
			(cond ((oddp listLength) (list (SUB-LIST L 0 (- halfLength .5)) (SUB-LIST L (- halfLength .5) (+ halfLength .5))))
				((evenp listLength) (list (SUB-LIST L 0 halfLength) (SUB-LIST L halfLength halfLength)))))))

;;; LIST2BTREE takes a list LEAVES and creates a binary tree.
;;; LEAVES is the list of elements we are creating into a binary tree.
;;; We return a binary tree, which is a tree where each node is either an internal node, which has two children, or a leaf, which is a single value. A child is represented by a list (L R), when L and R and the children of the node, and a leaf node is represented by an atom N. If there is an odd number, the R will be one longer than L.

;;; We use SPLIT-LIST as a helper function.
;;; Our base cases are either when LEAVES has 1 or 2 elements left. We create a single leaf node if 1, or an internal node if 2.
;;; Otherwise, we will use SPLIT-LIST to split the list LEAVES into two halves, and call LIST2BTREE recursively on each half.
;;; When there is an odd number, SPlIT-LIST puts the extra element into the second list, so we do not have to do anything extra to make sure that the right node is one larger or equal to the left node.

(defun LIST2BTREE (LEAVES)
	(cond ((= (length LEAVES) 1) (first LEAVES))
		((= (length LEAVES) 2) LEAVES)
		(t (let ((splitTree (SPLIT-LIST LEAVES)))
			(list (LIST2BTREE (first splitTree)) (LIST2BTREE(second splitTree)))))))

;;; BTREE2LIST does the inverse of LIST2BTREE. It takes a binary tree and creates a list of the values in the binary tree.
;;; TREE is the binary tree where we get the elements for the list.
;;; We return a list of the elements in TREE.

;;; The base case is the leaves, which is an atom (single number). We return a list that contains the leaf as its only value.
;;; Otherwise, it is an interal node, so we call BTREE2LIST recursively on the left node and right node and append the result of the right node onto the result of the left node.

(defun BTREE2LIST (TREE)
	(cond ((atom TREE) (list TREE))
		(t (APPEND (BTREE2LIST (first TREE)) (BTREE2LIST (second TREE))) )))