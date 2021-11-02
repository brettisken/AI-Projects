(in-package :cl-user)
(defpackage :n-queens
  (:documentation "Requires alexandria.  If you want to get JSON
output, also requires jonathan.")
  (:use :cl
	:alexandria)
  (:export :solutionp
           :generate-variables
           :generate-csp
	   :generate-solution))

(in-package :n-queens)

;;(ql:quickload :alexandria)

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

(defstruct solution
  csp
  assignment
  history)

(defun solutionp (assignment)
  "True iff ASSIGNMENT is a solution (that is, ASSIGNMENT is complete
and consistent) to the n-queens problem."
  ;; Here we just do a n^2 loop through the queens and make sure that
  ;; for each pair that they're not attacking each other horizontally
  ;; or along either of the diagonals.
  (loop for i from 1 to (length assignment) do
    (loop for j from (1+ i) to (length assignment) do
      (let ((pos-i (cadr (assoc (intern (format nil "Q~A" i) :keyword) assignment)))
            (pos-j (cadr (assoc (intern (format nil "Q~A" j) :keyword) assignment))))
        (when (or (= pos-i pos-j)
                  (= pos-j (+ pos-i (- j i)))
                  (= pos-j (- pos-i (- j i))))
          (return-from solutionp nil)))))
  t)

(defun generate-constraints (num-queens)
  ;; This the part that would be awful to do by hand - generate, for
  ;; each pair of queens, all the possible compatible assignments for
  ;; those two queens.
  (loop for i from 1 to num-queens nconc
    (loop for j from (1+ i) to num-queens collect
      (cons (list (intern (format nil "Q~A" i) :keyword)
                  (intern (format nil "Q~A" j) :keyword))
            (remove-if #'(lambda (pair)
                           (or (= (car pair) (cadr pair))
                               (= (cadr pair) (+ (car pair) (- j i)))
                               (= (cadr pair) (- (car pair) (- j i)))))
                       (alexandria:map-product #'list
                                               (loop for x from 1 to num-queens collect x)
                                               (loop for x from 1 to num-queens collect x)))))))
(defun generate-variables (num-queens)
  "Generate the queen/domain a-list for NUM-QUEENS queens.  This will
give the full domains - e.g. for each queen q,
(cdr (assoc q (generate-variables 8))) will be (1 2 3 4 5 6 7 8)."
  (mapcar #'(lambda (queen)
              (cons queen (loop for i from 1 to num-queens collect i)))
          (loop for j from 1 to num-queens collect (intern (format nil "Q~A" j) :keyword))))


(defun generate-csp (num-queens)
  "Generate a constraint satisfaction problem of size NUM-QUEENS,
where each queen's domain is all NUM-QUEES rows.  The queen variables
and domains are specified as a-lists, and the constraints are lists of
pairs where the first pair is the two queens involved in the
constraint, and the rest of the pairs represent all the compatible
assignments between those two queens."
  (list :variables (generate-variables num-queens)
        :constraints (generate-constraints num-queens)))

(defun generate-arcs (num-queens)
  "Returns a queue of arcs (:Q1 :Q2) (:Q2 :Q1) etc"
  (let (arcs)
    (setf arcs (make-queue)) ;fifo queue
    (loop for i from 1 to num-queens nconc
	 (loop for j from (1+ i) to num-queens do
	 (enqueue (list (intern (format nil "Q~A" i) :keyword) (intern (format nil "Q~A" j) :keyword)) arcs)
         (enqueue (list (intern (format nil "Q~A" j) :keyword) (intern (format nil "Q~A" i) :keyword)) arcs)))
  arcs))

(defun find-domain (xi domains)
  "Given a variable :Q1/:Q2/etc.. and a list of variables (domain list)
returns the given variables' domain list"
  (cond
    ((null domains) nil)
    ((not (equal nil (member xi (car domains)))) (member xi (car domains)))
    (t (find-domain xi (cdr domains)))))

(defun find-arc (xi xj constraints)
 "Returns the list of constraints for (xi xj) OR (xj xi)"
  (cond
    ((null constraints) nil)
    ((and (equal xi (caaar constraints)) (equal xj (cadaar constraints))) (car constraints))
    ((and (equal xj (caaar constraints)) (equal xi (cadaar constraints))) (car constraints))
    (t (find-arc xi xj (cdr constraints)))))

(defun revise (sol xi xj)
 (let (di dj arc can-revise revised)
   (setq di (find-domain xi (cadr (solution-csp sol))))
   (setq dj (find-domain xj (cadr (solution-csp sol))))
   (setq arc (find-arc xi xj (cadddr (solution-csp sol))))
   (setq revised nil) 
   (setq can-revise t) ;;if the current loop iteration detected a
   (dolist (dx (cdr di))
     ;;loop through the domain of the variable xi and check for a consistent element in xj domain
     ;;if an element is found which makes dx consistent DONT DELETE
     ;;Delete by default
	(dolist (constraint (cdr arc))
	     (cond 
	       ((and (equal (caar arc) xi) (equal (cadar arc) xj)) ;;Constraint (:xi :xj)
		(if (equal dx (car constraint))
		    (cond ((not (equal nil (member (cadr constraint) dj)))
		           (setq can-revise nil)))))
	       ((and (equal (caar arc) xj) (equal (cadar arc) xi)) ;;Constraint (:xj :xi)
		(if (equal dx (cadr constraint))
		    (cond ((not (equal nil (member (car constraint) dj)))
			   (setq can-revise nil)))))))
	(cond ((equal can-revise t)
	       (delete dx di) ;;remove the inconsistent domain element from CSP
	       ;;add to history the inconsistent domain element
	       (setf (nth dx (find-domain xi (car (solution-history sol)))) :x)
	       (setq revised t))
	      (t (setq can-revise t))))
   revised)) ;;return true if revised atleast once

(defun ac-3 (sol)
  (let (arcs arc closed valid)
    (setq arcs (generate-arcs (length (cadr (solution-csp sol)))))
    (setq closed (list))
    (setq valid t)
    (loop while (car arcs) do
	 (setq arc (dequeue arcs))
	 (push arc closed)
	 (cond ((revise sol (car arc) (cadr arc))
	       (if (equal (length (find-domain (car arc) (cadr (solution-csp sol)))) 1)
		   (progn
		     (setq valid nil)
		     (setq arcs nil))
		(dolist (xi arcs)
		 (if (equal (cadr xi) (car arc))
		     (enqueue xi arcs)))))))
    valid))

(defun minimum-remaining-values (csp vars)
  (let (min-val min-var)
    (setq min-val 999999)
    (dolist (d (cadr csp))
      (block break
      (cond ((< (length d) min-val)
	     (dolist (v vars)
	       (if (equal (car v) (car d))
		   (return-from break)))
	     (setq min-var (car d))
	     (setq min-val (length d))))))
    min-var))


(defun backtracking-search (csp)
  (backtrack
    (make-solution
     :csp csp
     :assignment (list)
     :history (list (mapcar #'(lambda (queen)
				(cons queen (loop for i from 1 to (length (second csp)) collect nil)))
			    (loop for j from 1 to (length (second csp)) collect (intern (format nil "Q~A" j) :keyword)))))))

(defun backtrack (sol)
  (if (equal (length (cadr (solution-csp sol))) (length (solution-assignment sol)))
      (return-from backtrack sol))
  (let (var di prev d-hist)
    (setf var (minimum-remaining-values (solution-csp sol) (solution-assignment sol))) ;;variable chosen
    (setf di (find-domain var (cadr (solution-csp sol)))) ;;domain list of variable chosen in csp
    (dolist (value (cdr di)) ;;loop on elements of domain
      (setf prev (copy-tree (second (solution-csp sol)))) ;;variables backup
      (push (list var value) (solution-assignment sol)) ;;add the new assignment
      (setf (second di) value) ;;set the value of variable in csp' variables
      (setf (cddr di) nil) ;;remove the other values of the domain
      (push (copy-tree (car (solution-history sol))) (solution-history sol)) ;;make a new history

      ;;set the newly placed queen in hist to q and all other values to x
      (setf d-hist (find-domain var (car (solution-history sol))))
      (loop for i from 1 to (length (cdr d-hist)) do
	       (setf (nth i d-hist) :x))
      (setf (nth value d-hist) :q)
      
      (if (ac-3 sol) ;;check arc conc. after assignment
	  (if (backtrack sol) ;;if still conc. try again
	      (return-from backtrack sol))) ;;correct solution found
      (pop (solution-history sol)) ;;remove bad assignment from history
      (setf (cadr (solution-csp sol)) prev) ;;remove bad assignment from csp
      (setf di (find-domain var (cadr (solution-csp sol)))) ;;fix pointer
      (pop (solution-assignment sol))) ;;remove bad assignment
    nil));;failed


(defun transpose (list-of-lists)
  (apply #'mapcar #'list list-of-lists))

(defun generate-solution (num domain-list)
  "Creates a csp, restricts the domains as needed and returns
the solution generated by backtracking-search. Then formats the history list
and returns the result"
  (let (sol csp final di)
    (setf csp (generate-csp num))
    (if (not (equal domain-list nil))
	(dolist (x domain-list)
	  (setf di (find-domain (car x) (second csp)))
	  (setf (second di) (cadr x))
	  (setf (cddr di) nil)))
    (setf sol (backtracking-search csp))
    (if (not sol)
	(return-from generate-solution nil))
    ;;remove variable keyword from rows and reverse the boards
    (setf final (cdr (mapcar #'(lambda (x)
			     (mapcar #'(lambda (y)
					 (cdr y))  
				     x))
			     (reverse (solution-history sol)))))
    ;;transpose the board lists so easier to display in djula
    (list :boards (mapcar #'(lambda (board)
			      (transpose board))
			  final))))







