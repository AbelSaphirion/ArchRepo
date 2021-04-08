(require "cl-csv")

(defmacro timewatch(&rest body)
  `(let ((first-timestop (get-universal-time)))
     ,@body
     (- (get-universal-time) first-timestop)))

(defmacro write-line-sym(&rest body)
  `(let ((ret ,@body))
     (write ret)
     (terpri)
     ret))

(defun export-csv (row-data file)
  (with-open-file (stream file :direction :output)
    (cl-csv:write-csv row-data :stream stream)))

(defun expt1 (b n)
  (if (= n 0)
      1
      (* b (expt1 b (- n 1)))))

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
    ((= n 1) b)
    (T (* b (fast-expt b (- n 1))))))

(let ((times '(100000 200000 300000 500000 1000000 1500000)))
  (export-csv
   (list (append '("Method") times)
         (append '("Recursion") (mapcar (lambda (x) (write-line-sym (timewatch (expt1 3 x)))) times))
         (append '("Tail Recursion") (mapcar (lambda (x) (write-line-sym (timewatch (expt2 3 x)))) times))
         (append '("Fast Expt") (mapcar (lambda (x) (write-line-sym (timewatch (fast-expt 3 x)))) times)))
   "test2.csv"))
