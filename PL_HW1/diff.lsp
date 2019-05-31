;; Function that computes longest common subsequence(lcs)
(defun lcs (a b)
  (cond
    ((or (null a) (null b)) nil)
    ((equal (car a) (car b))
       (cons (car a) (lcs (cdr a) (cdr b))))
    (t (longest (lcs a (rest b)) (lcs (rest a) b)))))

(defun longest (a b)
  (if (> (length a) (length b)) a b))

;; Compute diff info using lcs obtained from lcs function
(defun diff (list1 list2 difflist)
  (do ((dif (pop difflist)))
    ((and (null list1) (null list2) (null difflist)))
    (do ((e1 (pop list1) (pop list1)))
      ((equal e1 dif))
      (format t "~c[31m-~a~c[0m~%" #\ESC e1 #\ESC)
    )
    (do ((e2 (pop list2) (pop list2)))
      ((equal e2 dif))
      (format t "~c[32m+~a~c[0m~%" #\ESC e2 #\ESC)
    )
    (if
      (not (null dif))
      (format t " ~a~%" dif)
    )
    (setq dif (pop difflist))
  )  
)

;; Main function

;; Get file descriptors
(let (fd1 fd2 input1 input2)
(setq fd1 (open "./file1.txt"))
(setq fd2 (open "./file2.txt"))

;; Read in file line by line
(setq input1 ())
(setq input2 ())
(loop for line = (read-line fd1 nil nil)
  while line
  do (push line input1))
(loop for line = (read-line fd2 nil nil)
  while line
  do (push line input2))
(setq input1 (reverse input1))
(setq input2 (reverse input2))

;; Compute diff and output to console
(diff input1 input2 (lcs input1 input2)))