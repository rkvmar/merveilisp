;; init
(ql:quickload :lisp-unit2)
(in-package :cl-user)
(load "merveilisp.lisp")
(defpackage :merveilisp-tests
  (:use :cl :lisp-unit2)
  (:import-from :cl-user
                #:our-parse
                #:our-eval
                #:function-name-space))

(in-package :merveilisp-tests)

;; our-parse unit tests
(define-test merveilisp-tests::test-our-parse ()
  (assert-equal '(+ 1 2) (our-parse "(+ 1 2)"))
  (assert-equal '(- 5 3) (our-parse "(- 5 3)"))
  (assert-equal '(* 2 3 4) (our-parse "(* 2 3 4)"))
  (assert-equal '(/ 10 2) (our-parse "(/ 10 2)")))

;; our-eval unit tests
(define-test merveilisp-tests::test-our-eval-arithmetic ()
  (assert-equal 6 (our-eval '(+ 1 2 3)))
  (assert-equal 5 (our-eval '(- 10 5)))
  (assert-equal 24 (our-eval '(* 2 3 4)))
  (assert-equal 5 (our-eval '(/ 10 2)))
  (assert-equal 1 (our-eval '(% 5 2))))

;; edge testing
(define-test merveilisp-tests::test-edge-cases ()
  (assert-equal 0 (our-eval '(+)))
  (assert-equal 1 (our-eval '(*)))
  (assert-equal 5 (our-eval '(+ 5)))
  (assert-equal 5 (our-eval '(* 5))))

(defun run-all-tests ()
  "run all test suites and display results"
  (format t "~%running test suite...~%")
  (format t "================================~%")
  (print-summary (run-tests :package :merveilisp-tests)))

;; run all the tests
(run-all-tests)

;; example usage:
;; (run-all-tests)
;; (run-test 'merveilisp-tests::test-our-parse)
