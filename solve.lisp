;;; Solutions to the following arithmetic puzzle. See the included
;;; README file for more details.
;;;
;;; You need to fill in the gaps with the digits from 1 to 9 so that
;;; the equation makes sense, following the order of operations -
;;; multiply first, then division, addition and subtraction last.
;;;
;;; +----+    +----+----+----+    +----+
;;; |    |    |    |  - |    |    | 66 |
;;; +----+    +----+----+----+    +----+
;;; |  + |    |  x |    |  - |    |  = |
;;; +----+    +----+    +----+    +----+
;;; | 13 |    | 12 |    | 11 |    | 10 |
;;; +----+    +----+    +----+    +----+
;;; |  x |    |  + |    |  + |    |  - |
;;; +----+    +----+    +----+    +----+
;;; |    |    |    |    |    |    |    |
;;; +----+----+----+    +----+----+----+
;;; |  : |    |  + |    |  x |    |  : |
;;; +----+----+----+    +----+----+----+
;;;
;;; Here is an example repl session:
;;;
;;; CL-USER> (load "solve")
;;; T
;;; CL-USER> (run-tests)
;;; T
;;; CL-USER> (solve)
;;; ...list of solutions...


;;; Functions that can be found in alexandria. Including them here to
;;; avoid dependencies.

(defun permutations (list)
  "Return a list of all permutations of LIST."
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop for element in list
	      append (mapcar (lambda (l) (cons element l))
			     (permutations (remove element list)))))))

(defun set-equal (set1 set2)
  "Return T if SET1 is equal to SET2."
  (null (set-exclusive-or set1 set2)))

(defun factorial (n)
  "Return N!."
  (if (= n 0)
      1
      (* n (factorial (1- n)))))


;;; Helper functions

(defun solve-eqn (eqn values &optional (acc 0 acc-p))
  "Solve EQN using VALUES.

SOLVE-EQN is an interpreter for a tiny, infix-arithmetic expression
language. EQN is a list of expressions of the form TERM (OP TERM)+,
where each expression is reduced, then fed in as the left-hand TERM in
the next expression. You can think of SOLVE-EQN as doing a foldl over
the TERMs in EQN, where the next function to apply is given by
OP. TERM can be either a number or the special symbol '_, in which
case the value is taken as the CAR of VALUES. That is, a '_ in EQN
acts as a placeholder value, with the actual value taken from
successive elements of VALUES, which must be a list of numbers. OP
must be one of '(+ - * /).

A simple PEG-like grammar for EQN:

  EQN  <- TERM (OP TERM)+
  TERM <- '_' | INT
  OP   <- '+' | '-' | '*' | '/'
  INT  <- [0-9]+

And sample executions:

  CL-USER> (solve-eqn '(4) nil)
  4
  CL-USER> (solve-eqn '(1 + 2 / 3) nil)
  1
  CL-USER> (solve-eqn '(_ + _ / _) '(1 2 3))
  1
  CL-USER> (solve-eqn '(1 + _ - 1 / _) '(100 2))
  50
  CL-USER> (let ((*debugger-hook* (lambda (c h) (declare (ignore h)) (print c) (abort))))
	     (solve-eqn '(1 +) nil))

  #<SIMPLE-ERROR \"solve-eqn: malformed eqn\" {1003761983}>
"
  (cond ((and (null eqn) (null values)) acc)
	((null eqn) (error "solve-eqn: too many values"))
	((and (null values) (member '_ eqn)) (error "solve: too few values"))
	((not acc-p)
	 (cond ((eq '_ (car eqn)) (solve-eqn (cdr eqn) (cdr values) (car values)))
	       ((numberp (car eqn)) (solve-eqn (cdr eqn) values (car eqn)))
	       (t (error "solve-eqn: malformed eqn"))))
	(t (let ((valid-ops '(+ - * /))
		 (op (car eqn))
		 (term (cadr eqn)))
	     (cond ((eq nil term) (error "solve-eqn: malformed eqn"))
		   ((not (member op valid-ops)) (error "solve-eqn: invalid operation"))
		   ((eq '_ term) (solve-eqn (cddr eqn) (cdr values) (funcall op acc (car values))))
		   ((numberp term) (solve-eqn (cddr eqn) values (funcall op acc term)))
		   (t (error "solve-eqn: malformed eqn")))))))

(defun linear-precedence-constraint-satisfied-p (values)
  ;; Constraint to use if we interpret the puzzle as applying
  ;; operations linearly (same precedence, left associative).
  (let ((eqn '(_ + 13 * _ / _ + _ + 12 * _ - _ - 11 + _ * _ / _)))
    (= 76 (solve-eqn eqn values))))

(defun normal-precedence-constraint-satisfied-p (values)
  ;; Constraint to use if we interpret the puzzle using normal
  ;; operator precedence rules.
  (= 87 (apply (lambda (a b c d e f g h i)
		 (+ a (/ (* 13 b) c) d (* 12 e) (- f) (/ (* g h) i)))
	       values)))


;;; Main entry point

(defun solve (&optional (constraint-satisfied-p #'normal-precedence-constraint-satisfied-p))
  "Return a list of all solutions that satisfy
CONSTRAINT-SATISFIED-P.

SOLVE does a brute-force search over all 9! permutations of 1..9."
  (loop
     with digits = (loop for i from 1 to 9 collect i)
     for values in (permutations digits)
     if (funcall constraint-satisfied-p values)
     collect values))


;;; Test functions

(defun print-timings ()
  (format t "Solving with #'normal-precedence-constraint-satisfied-p~%")
  (time (solve #'normal-precedence-constraint-satisfied-p))
  (format t "Solving with #'linear-precedence-constraint-satisfied-p~%")
  (time (solve #'linear-precedence-constraint-satisfied-p))
  (values))

(defun dotest (constraint-satisfied-p expected-num-solutions)
  (let ((solutions (solve constraint-satisfied-p)))
    (and (= expected-num-solutions (length solutions))
	 (every constraint-satisfied-p solutions)
	 (every (lambda (s) (set-equal s '(1 2 3 4 5 6 7 8 9))) solutions))))

(defun test-normal-precedence ()
  (dotest #'normal-precedence-constraint-satisfied-p 136))

(defun test-linear-precedence ()
  (dotest #'linear-precedence-constraint-satisfied-p 152))

(defun test-solve-eqn ()
  (and (= 0 (solve-eqn nil nil))
       (= 4 (solve-eqn '(4) nil))
       (= 1 (solve-eqn '(1 + 2 / 3) nil))
       (= 1 (solve-eqn '(_ + _ / _) '(1 2 3)))
       (= 50 (solve-eqn '(1 + _ - 1 / _) '(100 2)))))

(defun test-factorial ()
  (and (= 1 (factorial 0))
       (= 1 (factorial 1))
       (= 2 (factorial 2))
       (= 6 (factorial 3))
       (= 24 (factorial 4))
       (= 120 (factorial 5))
       (= 362880 (factorial 9))))

(defun test-set-equal ()
  (and (set-equal '() '())
       (set-equal '(1) '(1))
       (set-equal '(1 2 3) '(1 2 3))
       (set-equal '(1 2 3 4) '(2 4 1 3))
       (not (set-equal '() '(1)))
       (not (set-equal '(1) '()))
       (not (set-equal '(1 2) '(1 1)))
       (not (set-equal '(1 2) '(1 2 3)))))

(defun test-permutations ()
  (and (null (permutations '()))
       (loop
	  for lst in '((1) (1 2 3) (1 2 3 4) (1 2 3 4 5) (1 2 3 4 5 6 7 8 9))
	  for perms = (permutations lst)
	  always (and (every (lambda (p) (set-equal p lst)) perms)
		      (= (length perms) (factorial (length lst)))))))

(defun run-tests ()
  (and (test-normal-precedence)
       (test-linear-precedence)
       (test-solve-eqn)
       (test-factorial)
       (test-set-equal)
       (test-permutations)))
