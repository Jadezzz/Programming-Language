;; Input: list
;; Output: 'TRUE' if the list is a palindrome, otherwise, 'FALSE'

(defun palindrome (list)
  (cond 
    ((equal list (reverse list)) 'True)
    (t 'False)))

;; Test data
;; (format t "~S~%" (palindrome '(a b c)))
;; (format t "~S~%" (palindrome '(m a d a m)))
;; (format t "~S~%" (palindrome '(cat dog)))
;; (format t "~S~%" (palindrome '()))
;; (format t "~S~%" (palindrome '(cat dog bird bird dog cat)))