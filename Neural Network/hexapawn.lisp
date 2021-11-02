(In-package :cl-user)

(defvar *initial-state* (list 0 -1 -1 -1 0 0 0 1 1 1))
(defvar *policy-table* (make-hash-table :test #'equal))

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

(defun state-to-array (state)
  (make-array '(3 3)
	      :initial-contents
	      (list (subseq state 0 3) (subseq state 3 6) (subseq state 6 9))))

(defun array-to-state (array)
  (let (state)
    (loop for i from 2 downto 0 do
     (loop for j from 2 downto 0 do
	  (push (aref array i j) state)))
    state))

(defun to-move (state)
  (car state))

(defun actions (state)
  (let (actions board)
    (setf board (state-to-array (cdr state)))
    (if (equal (to-move state) 0)
	(loop for i from 2 downto 1  do
	     (loop for j from 2 downto 0 do
		  (cond ((equal (aref board i j) 1)
			 (if (equal (aref board (1- i) j) 0)
			     (push (list :advance i j) actions))
			 (if (and (>= (1- j) 0) (equal (aref board (1- i) (1- j)) -1))
			     (push (list :capture-left i j) actions))
			 (if (and (<= (1+ j) 2) (equal (aref board (1- i) (1+ j)) -1))
			     (push (list :capture-right i j) actions))))))
	(loop for i from 0 to 1  do
	     (loop for j from 0 to 2 do
		  (cond ((equal (aref board i j) -1)
			 (if (equal (aref board (1+ i) j) 0)
			     (push (list :advance i j) actions))
			 (if (and (>= (1- j) 0) (equal (aref board (1+ i) (1- j)) 1))
			     (push (list :capture-left i j) actions))
			 (if (and (<= (1+ j) 2) (equal (aref board (1+ i) (1+ j)) 1))
			     (push (list :capture-right i j) actions)))))))
   
    actions))

(defun result (state action)
  (let (player board result yoffset)
    (setf board (state-to-array (cdr state)))
    (setf player (to-move state))
    (if (equal player 0)
	(setf yoffset -1)
	(setf yoffset 1))
    (case (car action)
      (:advance
       (rotatef (aref board (cadr action) (caddr action)) (aref board (+ (cadr action) yoffset) (caddr action))))
      (:capture-left
       (setf (aref board (+ (cadr action) yoffset) (1- (caddr action)))
	     (aref board (cadr action) (caddr action)))
       (setf (aref board (cadr action) (caddr action)) 0))
      (:capture-right 
       (setf (aref board (+ (cadr action) yoffset) (1+ (caddr action)))
	     (aref board (cadr action) (caddr action)))
       (setf (aref board (cadr action) (caddr action)) 0)))
    (setf result (array-to-state board))
    (if (equal player 0)
	(push 1 result)
	(push 0 result))
    result))

(defun is-terminal (state)
  (let (player board enemy)
    (setf player (to-move state))
    (cond ((equal player 0)
	   (setf board (subseq (cdr state) 6 9))
	   (setf enemy -1))
	  ((equal player 1)
	   (setf board (subseq (cdr state) 0 3))
	   (setf enemy 1)))
    (cond ((member enemy board)
	   t)
	  ((not (actions state))
	   t)
	  (t nil))))

(defun utility (state)
  (if (is-terminal state)
      (if (equal (to-move state) 0)
	  -1
	  1)
      0))
  	    
(defun minimax (state)
  (let (player util best-actions)
    (setf player (to-move state))
    (if (is-terminal state)
	(utility state)
	(progn
	  (dolist (action (actions state)) do
		  (setf util (minimax (result state action)))
		  (if (equal player 0)
		      (if (equal util 1)
			  (push action best-actions))
		      (if (equal util -1)
			  (push action best-actions))))
	  (if (car best-actions)
	      (setf (gethash (princ-to-string state) *policy-table*)
		    (list :state state :policy best-actions))
	      (setf (gethash (princ-to-string state) *policy-table*)
		    (list :state state :policy (actions state)))))))
  '())
	       
(defstruct neural-network
  layers
  learning
  activation
  aprime
  input
  output)

(defstruct layer
  weights
  biases
  cost
  output)

(defun generate-matrix (n m)
  (make-array (list n m)
	      :element-type 'double-float))

(defun generate-vector (n &optional initial)
  (if initial
      (make-array n
		  :initial-contents initial)
      (make-array n
		  :element-type 'double-float)))

(defun generate-layer (inputs neurons)
  "Builds a hidden layer for a neural network. Initializes
all weights to random numbers between -1 and 1. Edge weight 
matrix is [INPUT x NEURONS]"
  (let (random-state weights biases)
    (setf random-state (make-random-state t))
    (setf biases (generate-vector neurons))
    (setf weights (generate-matrix inputs neurons))
    (loop for j from 0 to (1- neurons) do
	 (setf (aref biases j) (1- (random 2.0d0)))
	 (loop for i from 0 to (1- inputs) do
	      (setf (aref weights i j) (1- (random 2.0d0)))))
    (make-layer
     :weights weights
     :biases biases
     :cost (generate-vector neurons)
     :output (generate-vector neurons))))


(defun generate-network (layers-list learning activation aprime)
  "Generates a neural network. Layers-list starts at first 
hidden layer and should go to the output layer. The layer 
format is '(in out). Where in is the number of input nodes
from the previous layer and out is number of nodes in this layer."
  (let (layers)
    (setf layers (make-queue))
    (dolist (layer layers-list)
      (enqueue (generate-layer (car layer) (cadr layer)) layers))
    (make-neural-network
     :layers layers
     :learning learning
     :activation activation
     :aprime aprime
     :input nil
     :output (layer-output (cadr layers)))))
  
(defun classify (nn input)
  "Builds the input layer (row vector). Then feeds the input foward
through every layer in the network. The final result is 
stored in the networks output."
  (let (in out)
    (setf in (generate-vector (list-length input) input))
    (setf (neural-network-input nn) in)
    (dolist (layer (car (neural-network-layers nn)))
      (setf out (layer-output layer))
      (foward-computation
       (layer-weights layer) (layer-biases layer) (neural-network-activation nn) in out)
      (setf in out))))

(defun relu (x)
  (if (> x 0.0d0)
      x
      0.0d0))

(defun relu-prime (x)
  (if (< x 0.0d0)
      0.0d0
      1.0d0))

(defun sigmoid (x)
  (coerce (expt (1+ (coerce (exp (coerce (* -1 x) 'double-float)) 'double-float)) -1) 'double-float))

(defun sigmoid-prime (x)
  (coerce (* x (- 1 x)) 'double-float))

(defun foward-computation (weights biases activation in out)
  "Multiplies the input row vector by the weight matrix [1xIN][INxOUT],
adds the bias, then runs the result through the activation function. 
The final result is stored in the output row vector."
  (destructuring-bind (rows cols) (array-dimensions weights)
    (declare (fixnum rows cols))
    (dotimes (j cols)
      (let ((sum 0.0d0))
        (declare (double-float sum))
        (setf (aref out j)
              (progn
                (dotimes (i rows)
                  (incf sum (* (aref weights i j) (aref in i))))
                (funcall activation (+ (aref biases j) sum))))))))

(defun backpropagation (nn expected)
  "Takes a neural-network and an expected output list.
Propates the cost back through the network then calculates 
the gradient descent on each layer and updates the edge 
weights and biases"
  (backward-cost expected nn)
  (backward-weights nn))

(defun output-cost (expected output-layer)
  "Calculate the cost for the output layer and stores it
in the last layers cost. Cost function is (exp - output)"
  (let (dim)
    (setf dim (array-dimension (layer-cost output-layer) 0))
    (loop for i from 0 to (1- dim) do
	 (setf (aref (layer-cost output-layer) i)
	       (- (car expected) (aref (layer-output output-layer)  i)))
	 (setf expected (cdr expected)))))

(defun backward-cost (expected nn)
  "Cost backpropigation"
  (let (reverse-layers layer)
    (setf reverse-layers (reverse (car (neural-network-layers nn))))
    (output-cost expected (car reverse-layers))
    (loop while (cdr reverse-layers) do
	 (setf layer (car reverse-layers))
	 (destructuring-bind (rows cols) (array-dimensions (layer-weights layer))
	   (declare (fixnum rows cols))
	   (dotimes (i rows)
	     (let ((sum 0.0d0))
	       (declare (double-float sum))
	       (setf (aref (layer-cost (cadr reverse-layers)) i)
		     (progn
		       (dotimes (j cols)
			 (incf sum (coerce
				    (* (aref (layer-weights layer) i j)
				       (aref (layer-cost layer) j))
				    'double-float)))
		       sum)))))
	 (setf reverse-layers (cdr reverse-layers)))))

(defun backward-weights (nn)
  "Calculates the gradient deltas and updates the
 edge weight and biases"
  (let (reverse-layers input layer)
    (setf reverse-layers (reverse (car (neural-network-layers nn))))
    (loop while reverse-layers do
	 (setf layer (car reverse-layers))
	 (if (cdr reverse-layers)
	     (setf input (layer-output (cadr reverse-layers)))
	     (setf input (neural-network-input nn)))
	 (destructuring-bind (rows cols) (array-dimensions (layer-weights layer))
	   (declare (fixnum rows cols))
	   (dotimes (j cols)
	     (setf (aref (layer-biases layer) j)
		   (+ (aref (layer-biases layer) j)
		      (* (neural-network-learning nn)
			 (* (aref (layer-cost layer) j)
			    (funcall (neural-network-aprime nn) (aref (layer-output layer) j))))))
	     (dotimes (i rows)
	       (setf (aref (layer-weights layer) i j)
		     (+ (aref (layer-weights layer) i j)
			(* (neural-network-learning nn)
			   (* (aref (layer-cost layer) j)
			      (* (funcall (neural-network-aprime nn) (aref (layer-output layer) j)) 
				 (aref input i)))))))))
	 (setf reverse-layers (cdr reverse-layers)))))

(defun adder-train ()
  "2 bit adder netwrok and training"
  (let (nn policy-array random-state rand)
    (setf random-state (make-random-state t))
    (setf nn (generate-network (list (list 2 2) (list 2 2)) .1 #'sigmoid #'sigmoid-prime))
    (setf policy-array
	  (make-array '(4 2)
		      :initial-contents (list (list (list 0 0) (list 0 0))
					      (list (list 0 1) (list 0 1))
					      (list (list 1 0) (list 0 1))
					      (list (list 1 1) (list 1 0)))))
    (dotimes (i 100000)
      (setf rand (random (array-dimension policy-array 0)))
      (classify nn (aref policy-array rand 0))
      (backpropagation nn (aref policy-array rand 1)))
    nn))

(defun optimal-move (player actions)
"Converts a list of actions to an optimal move list"
   (let (board result yoffset)
    (setf board (state-to-array (list 0 0 0 0 0 0 0 0 0)))
    (if (equal player 0)
	(setf yoffset -1)
	(setf yoffset 1))
    (dolist (action actions) do
	    (case (car action)
	      (:advance
	       (setf (aref board (+ (cadr action) yoffset) (caddr action)) 1))
	      (:capture-left
	       (setf (aref board (+ (cadr action) yoffset) (1- (caddr action))) 1))
	      (:capture-right 
	       (setf (aref board (+ (cadr action) yoffset) (1+ (caddr action))) 1)))
	    (setf result (array-to-state board)))
    result))

(defun hexa-train (layers-list learning activation aprime)
"Build policy table for hexapawn. Then randomly pick optimal moves
and train the network"
  (minimax *initial-state*)
  (let (state optimal nn policy-array policy-list random-state rand)
    (setf random-state (make-random-state t))
    (setf nn (generate-network layers-list learning activation aprime))
      (loop for value being the hash-values of *policy-table*  do
	   (setf state (getf value :state))
	   (setf optimal (optimal-move (to-move state) (getf value :policy)))
	   (push (list state optimal) policy-list)
	   (setf policy-array (make-array (list (list-length policy-list) 2)
					  :initial-contents policy-list)))
      (dotimes (i 100000)
	(setf rand (random (array-dimension policy-array 0)))
	(classify nn (aref policy-array rand 0))
	(backpropagation nn (aref policy-array rand 1)))
    nn))
