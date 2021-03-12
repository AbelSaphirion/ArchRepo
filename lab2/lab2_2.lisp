(defmacro timewatch(&rest body)
  `(let ((first-timestop (get-universal-time)))
     ,@body
     (print (- (get-universal-time) first-timestop))))

(defun expt1 (b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(defun expt2 (b n)
  (labels ((expt-iter (b counter product)
             (if (= counter 0)
                 product
                 (expt-iter b
                            (- counter 1)
                            (* b product)))))
  (expt-iter b n 1)))

(defun square (n) (* n n))
(defun even? (n) (= (rem n 2) 0))

(defun fast-expt (b n)
  (cond
    ((even? n) (square (fast-expt b (/ n 2))))
    ((= n 1) 1)
    (T (* b (fast-expt b (- n 1))))))
