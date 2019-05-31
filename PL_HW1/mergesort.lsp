(defun mergesort (numbers)
  (if (eq (length numbers) 1)
    ;; If list length = 1, return list
    numbers
    ;; Merge two lists
    (merge 'list
      ;; First list
      (mergesort (subseq numbers 0 (floor (length numbers) 2)))
      ;; Second list
      (mergesort (subseq numbers (floor (length numbers) 2)))
      ;; Merge in ascending order
      #'<
    )))

; Main function
(let 
  ((n (read))
  (numbers))
  (setf numbers
    (do 
      ((i 0 (+ i 1))
        (tmp nil)
      )
      ((>= i n) (reverse tmp))
      (setf tmp (cons (read) tmp))))
  (format t "~{~A ~}~%" (mergesort numbers)))