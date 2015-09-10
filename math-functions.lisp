(defun safe-exp (x)
	   (cond ((> (abs x) 30) (exp (* (/ x (abs x)) 30)))
		 (t (exp x))))

(defun learning-r (learning-ori n t2)
	   (* learning-ori (safe-exp (* -1 n (/ 1 t2)))))
	   
(defun small-random (x)
	   (- x (/ (random (* 1000d0 x)) 500d0)))
	   
(defmacro sigmoid (&optional (d-p nil))
	   (cond (d-p `#'(lambda (x) (* (/ 1d0 (+ 1d0 (safe-exp (* -1d0 x)))) (- 1d0 (/ 1d0 (+ 1d0 (safe-exp (* -1d0 x))))))))
		 (t   `#'(lambda (x) (/ 1d0 (+ 1d0 (safe-exp (* -1d0 x))))))))
		 
(defun RMSE (out target)
	   (let ((a (array-dimension out 1)) 
		 (b (array-dimension target 1))
		 (result 0d0))
	     (cond ((= a b)
		    (do ((i 0 (+ i 1)))
			((eql i a) (/ (expt result 0.5) a))
		     (setf result 
			   (+ result 
			      (expt (- (aref target 0 i) (aref out 0 i)) 2)))))
		   (t ':error))))
