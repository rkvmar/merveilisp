;; an example of what i'd like the user to be able to make!
(defun largest-prime-factor (num &optional (divisor 2))
  (cond
    ((eq num divisor) num)
    ((zerop (mod num divisor)) (largest-prime-factor (/ num divisor)))
    (t (largest-prime-factor num (1+ divisor)))))
