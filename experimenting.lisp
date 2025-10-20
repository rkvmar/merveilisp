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
       (print (split-sequence:split-sequence #\SPACE input))
       (repl)))))
(initialize)



;; next steps:

;; use lisps own parser (read) or (read-from-string), in this case
;; make a parse tree (really just a function that given an s-expression finds the deepest item)
;; make an evaluator (really just (is it +,-,/, or *, and then evaluate that with all arguments)
;; add in modulus to that ^ evaluator

;; defun, list, >, <, =/eq(zerop), let, if (cond)

;; priorities: +-*/, eq, if (cond), defun, let, list


;; if i dont have anything to eval/parse the function, let lisp do it, and write to a file to note what i should impliment

;; balance of steps 1-2 (programming) vs 3-4 (teaching) and focus on the latter


;; goals for THIS WEEKEND:
;; build all of the PRIORITY FUNCTIONS using: cons, nil, quote, eq, lambda, cond, assoc, atom, car, cdr, print, read, t
;; - pass any thing else into lisp
;; build an parse (binary (convert tree to binary)) tree to represent s-expressions


;; bonus idk: write a parser idk :3
;; - something that takes in anything of form:
;; ("x" val val) with <-- counting as a val
;; a val can be:
;; another s-expression (has parens), a number (regex), a string (has \"), or else a SYM


(print (assoc 3 '((1 1) (2 2) (3 3) (4 4))))
