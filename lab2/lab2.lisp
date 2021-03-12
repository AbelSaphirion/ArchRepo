(defmacro timewatch(&rest body)
  `(let ((first-timestop (get-universal-time)))
     ,@body
     (print (- (get-universal-time) first-timestop))))


(defun fib (n)
  (labels ((f (arr count)
             (if (eql count 0)
                 (third arr)
                 (f (list (second arr) (third arr) (+ (second arr) (third arr))) (- count 1)))))
    (f '(1 1 2) n)))
(defun get-fact (n)
  (labels ((f (x i)
             (if (< x 1)
                 (- i 1)
                 (f (/ x i) (+ i 1)))))
    (f n 1)))
(timewatch
 (get-fact (fib 200000)))

(defun compose (f1 f2)
  #'(lambda (n) (funcall f2 (funcall f1 n))))

(timewatch
 (funcall (compose #'fib #'get-fact) 200000))
