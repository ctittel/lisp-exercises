;; sfu LISP Tutorial 2
;; http://www.cs.sfu.ca/CourseCentral/310/pwfong/Lisp/2/tutorial2.html

;; Exercise: fast triangular
(defun fast-triangular-aux (N sum)
  "Calculate the triangular number with tail recursion"
  (if (= 0 N)
      sum
      (fast-triangular-aux (- N 1) (+ sum N))))

(defun fast-triangular (N)
  "Triangular number with tail-recursion"
  (fast-triangular-aux N 0))

;; Exercise: tail-recursive power function
(defun fast-power-aux (B E product)
  (if (= 0 E)
      product
      (fast-power-aux B (- E 1) (* B product))))

(defun fast-power (B E)
  "Fast power function: B to the power of E"
  (fast-power-aux B E 1))

;; Exercise: tail-recursive list length
(defun fast-list-length-aux (L sum)
  (if (null L)
      sum
      (fast-list-length-aux (rest L) (+ sum 1))))

(defun fast-list-length (L)
  (fast-list-length-aux L 0))

;; Exercise: apply List of functions on object X
(defun apply-func-list (L X)
  "Applies functions in L on X in reversed order"
  (if (null L)
      X
      (funcall (first L) (apply-func-list (rest L) X))))

;; Exercises: use apply-func-list to compute:
;;     10 times the fourth element of the list (10 20 30 40 50)
(print (apply-func-list (list #'(lambda (x) (* x 10)) #'fourth) '(10 20 30 40 50)))

;;     the third element of the second element in the list ((1 2) (3 4 5) (6))
(print (apply-func-list (list #'third #'second) '((1 2) (3 4 5) (6))))

;;     the difference between 10 and the length of (a b c d e f)
(print (apply-func-list (list #'(lambda (x) (- 10 x)) #'list-length) '(a b c d e f)))

;;     a list containing a list containing the symbol blah
(print (apply-func-list (list #'list #'list) 'blah))

;; Exercise: return a non-empty member of a list of lists
(defun non-empty-member (L)
  (if (null L)
      nil
      (if (null (first L))
	  (non-empty-member (rest L))
	  (first L))))

;; find-if test
(defun find-if-test (L)
  (find-if #'(lambda (x) (>= x 3)) L))

;; Exercise: using find-if to find members of a list of lists that have lenght longer or equal 3
(defun 3-or-more-members (L)
 (find-if #'(lambda (x) (>= (length x) 3)) L))

;; Exercise: use find-if to find member lists with even number of elements
(defun even-number-of-elements (L)
  (find-if #'(lambda (x) (evenp (length x))) L))

;; Exercise: use find-if to find a number in a list divisible by three
(defun divisible-by (number divisor)
  (if (= 0 (rem number divisor))
      t
      nil))

(defun find-number-divisible-3 (L)
  (find-if #'(lambda (x) (divisible-by x 3)) L))

;; Exercise: implement an own version of remove-if
(defun my-remove-if (F L)
  (if (null L)
      nil
      (if (funcall F (first L))
	  (my-remove-if F (rest L))
	  (cons (first L) (my-remove-if F (rest L))))))

;; Exercise: list-difference with remove-if
(defun list-difference (L1 L2)
  (remove-if #'(lambda (x) (and (member x L1) (member x L2))) (union L1 L2)))

;; Exercise: list-intersection with remove-if-not
(defun list-intersection (L1 L2)
  (remove-if-not #'(lambda (x) (and (member x L1) (member x L2))) (union L1 L2)))

;; Exercise: define list-min-max: returns the minimum and the maximum of a list
(defun list-min-max-aux (L min max)
  (if (null L)
      (values min max)
      (cond
	((> (first L) max) (list-min-max-aux (rest L) min (first L)))
	((< (first L) min) (list-min-max-aux (rest L) (first L) max))
	(t (list-min-max-aux (rest L) min max)))))

(defun list-min-max (L)
  (list-min-max-aux L (first L) (first L)))
