(defun prime(n)
  (let ((a 2) (lim (isqrt n)))
  (loop
    (if (equal n 2) (return 'true))
    (if (equal (mod n a) 0) (return 'false))
    (setq a (+ a 1))
    (when (> a lim) 
      (return 'true)))))

;; Test data
;; (format t "~S~%" (prime 2))
;; (format t "~S~%" (prime 239))
;; (format t "~S~%" (prime 999))
;; (format t "~S~%" (prime 17))