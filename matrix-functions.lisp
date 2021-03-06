;;;; matrix operation functions

;;;note that the idex 0 and 1 of the matrix is inverted (attempting to correct)

(defun matrix* (&rest matrices)
  (assert (cdr matrices) nil
          "You will achieve nothing by multiplying a single matrix.")
  (reduce
   #'(lambda (a b)
       (assert (= (array-dimension a 0) (array-dimension b 1)) nil
               "The number of rows in the first matrix should be the number ~
                of columns in the second matrix")
       (let ((result
              (make-array
               (list (array-dimension b 0) (array-dimension a 1))
               :initial-element 0d0)))
         (dotimes (i (array-dimension b 0) result)
           (dotimes (j (array-dimension a 1))
             (dotimes (k (array-dimension a 0))
               (incf (aref result i j) (* (aref a k j) (aref b i k))))))))
   matrices))

(defun matrix+ (&rest matrices)
	   (if (eql (cdr matrices) nil)
	       (car matrices)
	       (reduce 
		#'(lambda (a b)
		    (assert (and (= (array-dimension a 0) (array-dimension b 0))
				 (= (array-dimension a 1) (array-dimension b 1)))
			    nil
			    "Rows & Columns mismatch")
		    (let ((result
			   (make-array
			    (list (array-dimension a 0) (array-dimension a 1))
			    :initial-element 0d0)))
		      (dotimes (i (array-dimension a 0) result)
			(dotimes (j (array-dimension a 1))
			  (incf (aref result i j) (+ (aref a i j) (aref b i j)))))))
		matrices)))

(defun matrix- (&rest matrices)
	   (if (eql (cdr matrices) nil)
	       (car matrices)
	       (reduce 
		#'(lambda (a b)
		    (assert (and (= (array-dimension a 0) (array-dimension b 0))
				 (= (array-dimension a 1) (array-dimension b 1)))
			    nil
			    "Rows & Columns mismatch")
		    (let ((result
			   (make-array
			    (list (array-dimension a 0) (array-dimension a 1))
			    :initial-element 0d0)))
		      (dotimes (i (array-dimension a 0) result)
			(dotimes (j (array-dimension a 1))
			  (incf (aref result i j) (- (aref a i j) (aref b i j)))))))
		matrices)))

(defun transpose (matrix)
	   (let ((result 
		  (make-array
		   (list (array-dimension matrix 1) (array-dimension matrix 0))
		   :initial-element 0d0)))
	     (dotimes (i (array-dimension matrix 1) result)
	       (dotimes (j (array-dimension matrix 0))
		 (incf (aref result i j) (aref matrix j i))))))

(defun enlarge (matrix learning-rate)
           ;;times a number to a matrix
	   (let ((result (make-array (list
				      (array-dimension matrix 0)
				      (array-dimension matrix 1))
				      :initial-element 0d0)))
	     (dotimes (i (array-dimension matrix 0) result)
	       (dotimes (j (array-dimension matrix 1))
		 (setf (aref result i j) (* (aref matrix i j) learning-rate))))))

(defun diagonalise (M)
           ;;diagonalise a vector
	   (let ((result (make-array (list (array-dimension m 1)
					   (array-dimension m 1))
				     :initial-element 0d0)))
	     (dotimes (i (array-dimension m 1) result)
	       (incf (aref result i i) (aref m 0 i)))))

(defun Mrandom (Matrix ran-num)
           ;;mapping small random number to a matrix
	   (let ((result Matrix))
	     (dotimes (i (array-dimension result 0) result)
	       (dotimes (j (array-dimension result 1))
		 (setf (aref result i j) (small-random ran-num))))))
