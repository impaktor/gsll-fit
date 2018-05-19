(in-package :common-lisp-user)

;; turn off optimization:
(declaim (optimize debug))

(asdf:operate 'asdf:load-op :gsll)

(defconstant +parameters+ '(1.0 2.0 10.0 20.0 30.0))
(defconstant +dimensions+ 2 "i.e. number of variables of function")

(defun grid-to-list (grid)
  (let ((dim (first (grid:dimensions grid))))
    (loop
       for i from 0 below dim
       collect (grid:aref grid i))))

;; 1. Why does my function only get a single number, rather than a 2D (x y)
;; vector or foreign-array or list. What ever, two elemets, man!
;;
;; 2. what the heck is a settingp thingy, that is the second argument to f?
;; In C it's a void pointer, used to set parameters in the function.

(defun f (variables parameters-not-used-now-XXX)
  "Function I want to find minimum of"
  (destructuring-bind (p0 p1 p2 p3 p4) +parameters+
    (destructuring-bind (x y) (grid-to-list variables)
      (+  (* p2 (expt (- x p0) 2))
          (* p3 (expt (- y p1) 2))
          p4))))

;; make array of size +dimensions+:
(defvar init (GRID:MAKE-FOREIGN-ARRAY 'DOUBLE-FLOAT :dimensions +dimensions+ :initial-element 1.0d0))
(defvar step-size (GRID:MAKE-FOREIGN-ARRAY 'DOUBLE-FLOAT :dimensions +dimensions+ :initial-element .01d0))

(let ((fit (GSLL:MAKE-MULTI-DIMENSIONAL-MINIMIZER-F
            GSLL:+SIMPLEX-NELDER-MEAD-ON2+ +dimensions+
            #'f             ; function I want to minimize
            init            ; initial variable values
            step-size       ; step sizes to test in minimization procedure
            )))
  (do ((iter 0 (1+ iter))
       (status 0 (gsll:iterate fit))    ; winging it! Do the fit
       (is-good-enough-p (gsll:min-test-size (gsll:size fit) 1e-3)))
      ((or (equal status 1)             ; todo: use equal? is 1 that it is = error? xxx
           (< 100 iter)                 ; bail out if too many steps
           is-good-enough-p)
       (format t "~a~a~%~a~%~a~%~a~%" ; run when finished
               status
               iter
               fit-params
               fit-errors
               fit-gradient))))
