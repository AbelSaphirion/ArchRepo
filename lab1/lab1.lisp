(defmacro timewatch(&rest body)
  `(let ((first-timestop (get-universal-time)))
     ,@body
     (print (- (get-universal-time) first-timestop))))

(timewatch
 (let ((arr `(1 1 2)))
   (dotimes (i 1000000)
     (setf arr (list (second arr) (third arr) (+ (second arr) (third arr)))))))

(timewatch
 (defun fib (n)
   (labels ((f (arr count)
              (if (eql count 0)
                  (list (second arr) (third arr) (+ (second arr) (third arr)))
                  (f (list (second arr) (third arr) (+ (second arr) (third arr))) (- count 1)))))
     (f '(1 1 2) n)))
 (fib 1000000))

(timewatch
 (defun fib2 (n)
   (if (< n 3) 1 (+ (fib2 (- n 2)) (fib2 (- n 1)))))
 (fib2 50))
