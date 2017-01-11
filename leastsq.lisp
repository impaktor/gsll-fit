(in-package :common-lisp-user)

(asdf:operate 'asdf:load-op :gsll)

(defconstant +parameters+ '(1.0 2.0 10.0 20.0 30.0))

(defun f (variables parameters)
  "Function I want to fit to"
  (destructuring-bind (p0 p1 p2 p3 p4) parameters
    (destructuring-bind (x y) variables
      (+  (* p2 (expt (- x p0) 2))
          (* p3 (expt (- y p1) 2))
          p4))))


;; Find how to use the following:
;; ==============================
;; LISP: GSLL:+SIMPLEX-NELDER-MEAD-ON2+
;; GSLL:MAKE-MULTI-DIMENSIONAL-MINIMIZER-F
;; (REINITIALIZE-INSTANCE GSLL:MULTI-DIMENSIONAL-MINIMIZER-F)
;; GSLL:ITERATE
;; GSLL:SIZE
;; GSLL:MIN-TEST-SIZE
