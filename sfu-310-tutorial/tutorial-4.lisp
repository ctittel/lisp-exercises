;; Exercise: create a global stack abstraction, with pushing and poping
(defparameter *stack* '() "Global stack")

(defun my-push (x)
  (if (null *stack*)
      (setf *stack* (list x))
      (setf *stack* (cons x *stack*))))

(defun my-pop ()
  (if (null *stack*)
      nil
      (let ((r (first *stack*)))
	(setf *stack* (rest *stack*))
	r)))

;; Exercise: define encapsulated stack
(setf list-of-functions (let ((stack nil))
			  (list #'(lambda (x) (setf stack (cons x stack)))
				#'(lambda () (let ((x (first stack)))
					       (setf stack (rest stack))
					       x))
				#'(lambda () stack))))

(setf mypush (first list-of-functions))
(setf mypop (second list-of-functions))
(setf mystack (third list-of-functions))

					; functions are called via (funcall mypush "test") etc.

;; Exercise: Make the encapsulated stack abstraction as a poor man's object
(defun make-stack () (let ((stack nil))
			  (list #'(lambda (x) (setf stack (cons x stack)))
				#'(lambda () (let ((x (first stack)))
					       (setf stack (rest stack))
					       x))
				#'(lambda () stack))))

(defun push-stack (stack-object x) (funcall (first stack-object) x))
(defun pop-stack (stack-object) (funcall (second stack-object)))
(defun get-stack (stack-object) (funcall (third stack-object)))
