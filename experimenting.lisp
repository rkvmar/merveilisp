(ql:quickload "cl-ppcre")
(ql:quickload :split-sequence)
(setq tst-str "(* 234 2)")
;;(setq num-regex "-?[0-9]+")
;;(setq op-regex "[\\+\\-\\/\\*]")
(defun exp-detect (input
		   &optional (preface "(") (infix #\SPACE) (postface ")")
		     (num-regex "-?[0-9]+")) ;;(op-regex "[\\+\\-\\/\\*]"))
  "given an INPUT string, returns all exp"
  (print input)
  ;; error catch by regex parens
  (cond
    ((equal input (car (ppcre:all-matches-as-strings num-regex input))) (print 1) (parse-integer input))
    ((and
      (eq 1 (length (ppcre:all-matches-as-strings num-regex input)))
      (equal (subseq input (length (car (ppcre:all-matches-as-strings num-regex input))))
	     (car (ppcre:all-matches-as-strings
		   (concatenate 'string "\\" postface "+") input))))
     (print 2) (parse-integer input))
    ((listp input)
     (print 3)
     (append
      (list (exp-detect (car input)))
      (exp-detect (cdr input))))
    ((equal (subseq input 0 1) preface) (print 4)
     (let ((elements (split-sequence:split-sequence infix input)))
       (append (list (subseq (car elements) 1)) (exp-detect (cdr elements)))))))
;; NOTE! NOT TAIL RECURSIVE

(print (exp-detect "(+ 1 2)"))
   
(defun list-depth (lst depth)
  (cond
    ((> depth 1) (list-depth (car (reverse lst)) (1- depth)))
    (t lst)))

(setq tst-val (list "+" (list "*" 3 4) (list "+" 2)))

(setf (car (list (list-depth tst-val 2))) (append (list-depth tst-val 2) '(3)))


(print (list-depth (list "+" (list "*" 3 4) (list "+" 2)) 2 3))

(print (ppcre:all-matches-as-strings op-regex tst-str))

     
(defun initialize ()
  (print "Welcome to Merveilisp!
 .,. to exit.")
  (repl))
(defun repl ()
  (let ((input (read-line)))
    (cond
      ((equal input ".,."))
      (t
       (print input)
       (repl)))))
(initialize)
