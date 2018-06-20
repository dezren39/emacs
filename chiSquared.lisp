(defun chiSquared (expected observed) "chi squared test"
       (/ (expt (- observed expected) 2) (float expected)))

(defun chiSquaredL (pair) "chi squared test"
       (chiSquared (pop pair) (pop pair)))

(defun makePairs(listIn) "list of things into list of lists of 2 things"       
       (if (cadr listIn)	 
	   (cons (list (pop listIn) (pop listIn)) (makePairs listIn))))

(defun chiSquaredTest (&rest pairs) "do a chi squared test"
       (let ((holder 0) result)
	 (dolist (pair (makePairs pairs) result)	
	   (setq result
		 (setq holder
		       (+ (chiSquaredL pair) holder))))))

(chiSquaredTest 39 56 39 44 42 47 72 62 60 53 48 38)



(defun simpleChiSquaredTest (expected &rest observedList) "do a chi squared test"
       (let ((holder 0) result)
	 (dolist (observed observedList result)
	   (setq result
		 (setq holder
		       (+ (chiSquared expected observed)holder))))))
