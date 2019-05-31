;; Original recursion
(defun fib1 (n)
  (if (< n 2)
    n
    (+ (fib1 (- n 1)) (fib1 (- n 2)))))

;; Tail recursion
(defun fib2 (n)
  (defun fib-tailrec(index prev current)
    (cond ((= index 0) current)
          (t (fib-tailrec (- index 1) (+ prev current) prev))))
  (fib-tailrec n 1 0))

;; Test data
;; (format t "~D ~D~%" (fib1 7) (fib2 7))