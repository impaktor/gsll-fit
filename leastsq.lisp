(in-package :common-lisp-user)

(asdf:operate 'asdf:load-op :gsll)

(defconstant +parameters+ '(1.0 2.0 10.0 20.0 30.0))

(defconstant +dimensions+ 2 "i.e. number of variables of function")

(defun f (variables parameters)
  "Function I want to find minimum of"
  (destructuring-bind (p0 p1 p2 p3 p4) parameters
    (destructuring-bind (x y) variables
      (+  (* p2 (expt (- x p0) 2))
          (* p3 (expt (- y p1) 2))
          p4))))

(let ((fit (GSLL:MAKE-MULTI-DIMENSIONAL-MINIMIZER-F
            GSLL:+SIMPLEX-NELDER-MEAD-ON2+ +dimensions+
            #'f            ; function I want to minimize, ("#'"?) xxx
            '(.1 .1)       ; inital value (should this be a list?) xxx
            '(.01 .01)     ; step size (should this be a list?) xxx
            )))
  (do ((iter 0 (1+ iter))
       (status 0 (gsll:iterate fit))    ; winging it! Do the fit
       (is-good-enough-p (gsll:min-test-size (gsll:size fit) 1e-3)))
      ((or (equal status 1)             ; todo: use equal? is 1 that is = error? xxx
           (< 100 iter)                 ; bail out if too many steps
           is-good-enough-p)
       (format t "~a~a~%~a~%~a~%~a~%" ; run when finished
               status
               iter
               fit-params
               fit-errors
               fit-gradient))))


;; Find how to use the following:
;; ==============================
;; LISP: GSLL:+SIMPLEX-NELDER-MEAD-ON2+

;; (GSLL:MAKE-MULTI-DIMENSIONAL-MINIMIZER-F type dimension &optional function initial step-size)
;; (GSLL:ITERATE object)
;; (GSLL:SIZE object)
;; (GSLL:min-test-size size absolute-error)

;; (REINITIALIZE-INSTANCE GSLL:MULTI-DIMENSIONAL-MINIMIZER-F)
