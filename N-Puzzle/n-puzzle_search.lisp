;;Brett Isken
(in-package :cl-user)

(defvar *goal* #2A((nil 1 2)(3 4 5)(6 7 8)))
(defvar *puzzle-0* #2A((3 1 2)(7 nil 5)(4 6 8)))
(defvar *puzzle-1* #2A((7 2 4)(5 nil 6)(8 3 1)))
(defvar *puzzle-2* #2A((6 7 3)(1 5 2)(4 nil 8)))
(defvar *puzzle-3* #2A((nil 8 6)(4 1 3)(7 2 5)))
(defvar *puzzle-4* #2A((7 3 4)(2 5 1)(6 8 nil)))
(defvar *puzzle-5* #2A((1 3 8)(4 7 5)(6 nil 2)))
(defvar *puzzle-6* #2A((8 7 6)(5 4 3)(2 1 nil)))

(defvar *15-goal* #2A((nil 1 2 3)(4 5 6 7)(8 9 10 11)(12 13 14 15)))
(defvar *15-puzzle-0* #2A((1 5 2 3)(4 6 10 7)(8 13 9 11)(12 nil 14 15))) ;;Easy puzzle works
(defvar *15-puzzle-1* #2A((13 10 11 6)(5 7 4 8)(1 12 14 9)(3 15 2 nil))) ;;this one is not solvable
(defvar *15-puzzle-2* #2A((13 10 11 6)(5 7 4 8)(2 12 14 9)(3 15 1 nil))) ;;Didn't get this to work


;ANSI_Common_Lisp_-_Paul_Graham
(defun make-queue () (cons nil nil))
(defun enqueue (obj q)
  (if (null (car q))
      (setf (cdr q) (setf (car q) (list obj)))
      (setf (cdr (cdr q)) (list obj)
	    (cdr q) (cdr (cdr q))))
  (car q))
(defun dequeue (q)
  (pop (car q)))


;Given priority queue implementation
(defun make-heap (&key (predicate #'<) (key #'identity) (initial-size 4))
  "Returns an empty priority queue.  Items in the queue will be
ordered by PREDICATE on their keys.  KEY is a function that takes an
item and gives back the key of that item."
  (list :heap (make-array initial-size :fill-pointer 0)
        :predicate predicate
        :key key))

(defun emptyp (queue)
  (zerop (fill-pointer (getf queue :heap))))

(defun parent (index)
  (if (oddp index) (/ (1- index) 2) (1- (/ index 2))))

(defun reheap-up (queue from)
  (do ((curr from (parent curr)))
      ((or (zerop curr)
           (not (funcall (getf queue :predicate)
                         (funcall (getf queue :key)
                                  (aref (getf queue :heap) curr))
                         (funcall (getf queue :key)
                                  (aref (getf queue :heap) (parent curr))))))
       queue)
    (rotatef (aref (getf queue :heap) (parent curr))
             (aref (getf queue :heap) curr))))

(defun push-heap (obj queue)
  (when (= (fill-pointer (getf queue :heap))
           (array-dimension (getf queue :heap) 0))
    (adjust-array (getf queue :heap)
                  (* 2 (fill-pointer (getf queue :heap)))))
  (incf (fill-pointer (getf queue :heap)))
  (setf (aref (getf queue :heap)
              (1- (fill-pointer (getf queue :heap))))
        obj)
  (reheap-up queue (1- (fill-pointer (getf queue :heap)))))

(defun pop-heap (queue)
  (labels ((smaller-child (index)
             (let* ((left-child (1+ (* 2 index)))
                    (right-child (1+ left-child)))
               (cond ((= left-child (1- (length (getf queue :heap)))) left-child)
                     ((< right-child (length (getf queue :heap)))
                      (if (funcall (getf queue :predicate)
                                   (funcall (getf queue :key)
                                            (aref (getf queue :heap) left-child))
                                   (funcall (getf queue :key)
                                            (aref (getf queue :heap) right-child)))
                          left-child
                          right-child))))))
    (decf (fill-pointer (getf queue :heap)))
    (rotatef (aref (getf queue :heap) 0)
             (aref (getf queue :heap)
                   (fill-pointer (getf queue :heap))))
    (do* ((smaller-child (smaller-child 0) (smaller-child smaller-child))
          (curr 0 (parent (or smaller-child 0))))
         ((or (null smaller-child)
              (not (funcall (getf queue :predicate)
                            (funcall (getf queue :key)
                                     (aref (getf queue :heap) smaller-child))
                            (funcall (getf queue :key)
                                     (aref (getf queue :heap) curr)))))
          (prog1 (aref (getf queue :heap) (fill-pointer (getf queue :heap)))
            (when (= (fill-pointer (getf queue :heap))
                     (/ (array-dimension (getf queue :heap) 0) 4))
              (adjust-array (getf queue :heap)
                            (* 2 (fill-pointer (getf queue :heap)))))))
      (rotatef (aref (getf queue :heap) smaller-child)
               (aref (getf queue :heap) curr)))))


;;Creates deep copy of an array
(defun copy-array (array)
   (let* ((dimensions (array-dimensions array))
         (new-array (make-array dimensions)))
    (dotimes (i (array-total-size array))
      (setf (row-major-aref new-array i)
            (row-major-aref array i)))
    new-array))

;;the node struct used for tree search
(defstruct node
  state
  parent
  depth
  action
  total-cost)

;helper function to print the solution node linked list
(defun print-solution (node)
  (let* (solution)
    (setq solution (list (list ':ACTION-SEQUENCE) (list ':STATE-SEQUENCE) (list ':PATH-COST (node-depth node))))
    (loop while (node-parent node)
       do
	 (push (node-action node) (cdar solution))
	 (push (node-state node) (cdadr solution))
	 (setq node (node-parent node)))
    (push (node-state node) (cdadr solution))
    solution))

(defun possible-actions (board dim)
  (let (actions)
   (loop for i from 0 to (- dim 1) do
	(loop for j from 0 to (- dim 1)  do
	 ;up
	 (if (and (>= (- i 1) 0) (not (aref board (- i 1) j)))
	    (push  (list :up i j) actions))
	 ;left
	 (if (and (>= (- j 1) 0) (not (aref board i (- j 1))))
	    (push (list :left i j) actions))
	 ;down
	 (if (and (< (+ i 1) dim) (not (aref board (+ i 1) j)))
	     (push (list :down i j) actions))
	 ;right
	 (if (and (< (+ j 1) dim) (not (aref board i (+ j 1))))
	     (push (list :right i j) actions))))
     actions))

;creates deep copy of input board, swaps action i j in the action direction 
(defun result (action board)
  (let (result)
    (setq result (copy-array board))
    (case (car action)
      (:up (rotatef (aref result (cadr action) (caddr action)) (aref result (- (cadr action) 1) (caddr action))))
      (:down (rotatef (aref result (cadr action) (caddr action)) (aref result (+ (cadr action) 1) (caddr action))))
      (:left (rotatef (aref result (cadr action) (caddr action)) (aref result (cadr action) (- (caddr action) 1))))
      (:right (rotatef (aref result (cadr action) (caddr action)) (aref result (cadr action) (+ (caddr action) 1)))))
    result))

(defun expand (node dim)
  (let (states)
    (loop for action in (possible-actions (node-state node) dim)
       do (push (make-node
		 :state (result action (node-state node))
		 :parent node ;;link back to parent
		 :depth (1+ (node-depth node)) ;;depth is set to 1 plus its parent
		 :action action)
	   states))
    states))

(defun DLS(board goal limit)
  (let* (opens node result)
    (setq opens (cons (make-node :state board :depth 0)  nil))
    (loop while (car opens)
	 do (setq node (pop opens))
	 (cond ((equalp (node-state node) goal)(setq result node) (setq opens nil))
	       ((> (node-depth node) limit) (setq result :cutoff))
               ((not nil) (loop for child in (expand node 3) do (push child opens)))))
    result))

(defun IDS(board goal)
  (let* (depth result)
    (setf depth 0)
    (loop
       do 
	 (setf result (DLS board goal depth))
	 (if (not (eq result :cutoff)) (return))
	 (setf depth (+ depth 1)))
    result))

(defun BFS(board goal)
  (let (opens closed node result)    
    (setf opens (make-queue)) ;opens is fifo queue
    (enqueue (make-node :state board :depth 0) opens)
    (setf closed (make-hash-table :test 'equal)) ;closed is hashtable using only the board state hash
    (setf (gethash (prin1-to-string board) closed) 0)
    (setf result :failure)
    (loop while (car opens)
       do (setf node (dequeue opens))
	 (loop for child in (expand node 3)
	    do (cond
		 ((equalp (node-state child) goal) (setf result child)(setq opens nil)(return))
		 ((not (gethash (prin1-to-string (node-state child)) closed)) ;check closed if state exists
		  (setf (gethash (prin1-to-string (node-state child)) closed) 0) ;add child to closed and open
		  (enqueue child opens)))))
   result))

(defun manhattan-distance(board dim)
  (let* (manSum tile dx dy)
    (setf manSum 0)
    (loop for i from 0 to (- dim 1) do
	 (loop for j from 0 to (- dim 1)  do
	      (setf tile (aref board i j))
	      (if (not (eq tile nil))
		  (progn
		    (setf dx (- i (floor (/ tile dim))))
		    (setf dy (- j (mod tile dim)))	
		    (setf manSum (+ manSum (+ (abs dx) (abs dy))))))))
    manSum))

(defun misplaced-tiles(board dim)
  (let* (misSum properTile)
    (setf misSum -1) ;;tile 0 0 will always come back as misplaced
    (setf properTile 0) ;;compare to curr tile, if not eq add 1 to sum
    (loop for i from 0 to (- dim 1) do
	(loop for j from 0 to (- dim 1)  do
	     (if (not (eq properTile (aref board i j))) (setf misSum (+ misSum 1)))
	     (setf properTile (+ properTile 1))))
    misSum))

(defun A*(board goal hn) 
  (let (opens closed node result)    
    (setf opens (make-heap :key #'node-total-cost)) ;open is a min-heap
    (push-heap (make-node :state board :depth 0 :total-cost 0) opens)
    (setf closed (make-hash-table :test 'equal)) ;closed is same hashtable as BFS
    (setf (gethash (prin1-to-string board) closed) 0)
    (setf result :failure)
    (loop while (cadr opens)
       do (setf node (pop-heap opens)) 
	   (cond ((equalp (node-state node) goal) (setf result node) (setq opens nil)(return)))
	   (loop for child in (expand node (array-dimension goal 0))
	      do
		;set new child's h value
		(setf (node-total-cost child) (+ (node-depth child) (funcall hn (node-state child) (array-dimension goal 0))))
		(cond
		  ((not (gethash (prin1-to-string (node-state child)) closed)) ;check if already visited
		     (setf (gethash (prin1-to-string (node-state child)) closed) (node-total-cost child))
		     (push-heap child opens)))))
   result))
