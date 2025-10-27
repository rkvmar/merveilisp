;;define function-name-space up here for testing purposes
(defvar function-name-space nil)

(defun our-parse (str)
  "takes in a string STR and converts it into an S-expression"
  (read-from-string str))
;;letting lisp do the hard work- since I couldn't build a good parser on my own
;;so, TODO: given a string in the form of "(WORD VAL VAL)" turn it into (WORD VAL VAL)
;; (surprisingly hard! mostly because it may be "(WORD (WORD VAL VAL VAL) VAL)")

(defun our-defun (function-name lambda-list &rest body)
  "serves as a defun! user passes in a NAME, LAMBDA-LIST (inputs), and a BODY (list of arguments). has side effects (effecting global list of functions)."
  (setq function-name-space (append
			     function-name-space
			     (list function-name `(lambda (,@lambda-list) ,@body)))))

(defun missing-function-logging (name &optional (file-name "missing-functions.txt"))
  "writes NAME to file FILE-NAME. expected use is for flagging functions to make. has side effects"
  (with-open-file (f file-name
                     :direction :output
                     :if-exists :append
                     :if-does-not-exist :create)
    (write-sequence (concatenate 'string (string name) "
") f)))

(defun our-eval (s-expression)
  "takes in an S-expression and parses it"
  (cond
    ((and (every #'atom (cdr s-expression))
	  (some (lambda (x) (eq x (car s-expression)))
		(list '+ '- '* '/ '% 'defun)))
	  ;;checks to make sure there aren't any nestled lists
	  ;; i.e. works with (+ 1 2 3) but not (+ 1 (+ 2 3) 4)
     (let ((a (car s-expression)) (b (cdr s-expression)))
       (cond
	 ;; there's got to be a way to simplify this but i can't figure out how
	 ;; so, TODO: add macros/mapcars?
	 ((eq '+ a) (apply '+ b))
	 ((eq '- a) (apply '- b))
	 ((eq '* a) (apply '* b))
	 ((eq '/ a) (apply '/ b))
	 ((eq '% a) (apply 'mod b))
	 ((eq 'defun a) (our-defun (car b) (cadr b) (cddr b)))
	 )))
    ((assoc (car s-expression) function-name-space) ;;if this is a function that was defined with defun
     (funcall (cdr (assoc (car s-expression) function-name-space)) (cdr s-expression)))
     ;; TODO: more error handling
    (t
     (missing-function-logging (car s-expression))
     ;; if a function in my language uses Lisp's evaluator to work, write it down in a file
     ;; so i can impliment that myself later
       (eval s-expression))))

(defun repl ()
  "the core read-eval-print-loop system of the code"
  (let ((input (read-line))) ;;input from user
    (cond
      ((equal input ".,.") (format t "
Exiting.")) ;;exit case
      (t
       (print (our-eval (our-parse input)))
       (format t "
>> ")
       (repl)))))

(defun initialize ()
  "initalizes an instance of merveilisp"
  (setf function-name-space nil) ;;reset the global list of all functions
  (format t "

Welcome to Merveilisp!
 .,. to exit.

>> ")
  (repl))
