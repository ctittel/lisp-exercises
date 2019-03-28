;; tutorial 1 of sfu cs 310

;; Exercise: triangular number
(defun triangular (N)
  (if (= N 1)
      1
      (+ N (triangular (- N 1)))))

;; Exercise: Binomial Coefficient
(defun B (n r)
  (if (or (= n r) (= 0 r))
      1
      (+ (B (- n 1) (- r 1))
	 (B (- n 1) r))))

;; Exercise: Recursive List Length
(defun recursive-list-length (L)
  (if (null L)
      0
      (+ 1 (recursive-list-length (rest L)))))

;; Exercise: Recursive List sum
(defun recursive-list-sum (L)
  (if (null L)
      0
      (+ (first L) (recursive-list-sum (rest L)))))

;; Exercise: Recursive last
(defun recursive-last (L)
  (if (null (rest L))
      (first L)
      (recursive-last (rest L))))

;; Exercise: Recursive butlast
(defun recursive-butlast (L)
  (if (null (rest L))
      nil
      (cons (first L) (recursive-butlast (rest L)))))

;; Exercise: Recursive union
(defun recursive-union (L1 L2)
  "Recursive implementation of set union"
  (cond
    ((null L1) L2)
    ((member (first L1) L2) (recursive-union (rest L1) L2))
    (t (recursive-union (rest L1) (cons (first L1) L2)))))

;; Exercise: Recursive difference
(defun recursive-difference (L1 L2)
  "Recursive implementation of set difference"
  (union (recursive-difference-aux L1 L2) (recursive-difference-aux L2 L1)))

(defun recursive-difference-aux (L1 L2)
  (cond
    ((null L1) nil)
    ((member (first L1) L2) (recursive-difference-aux (rest L1) L2))
    (t (cons (first L1) (recursive-difference-aux (rest L1) L2)))))
