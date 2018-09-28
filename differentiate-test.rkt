
(require rackunit)
(require rackunit/text-ui)
(require "differentiate.rkt")

(define (test-expr1) 'x)
(define (test-expr2) 'a)
(define (test-expr3) '(+ x 5 x))
(define (test-expr4) '(* x x))
(define (test-expr5) '(expt x 2))
(define (test-expr6) '(expt x n))
(define (test-expr7) '(+ (* 3 (expt x 3)) (* -2 x) 5))

(define (test-mc1) '(exp x))
(define (test-mc2) '(ln x))
(define (test-mc3) '(ln (expt x 2)))
(define (test-mc4) '(exp (expt x 2)))
(define (test-mc5) '(expt (ln x) 2))

(define (test-mf1) '(log x 10))
(define (test-mf2) '(sin x))
(define (test-mf3) '(cos x))
(define (test-mf4) '(tan x))
(define (test-mf5) '(sec x))
(define (test-mf6) '(/ x (sin x)) )
(define (test-mf7) '(sqrt x))

(define (test-simplify-1) '(+ 1 0 x -1))
(define (test-simplify-2) '(* (+ 1 (* 3 2)) x))
(define (test-simplify-3) (diff 'x '(* 3 (expt x 2))))

# tests basic rules
(define basic-test
  (test-suite
   "Tests for basic rules"
   (check-equal? (diff 'x (test-expr1)) 1 "Derivative of a variable")
   (check-equal? (diff 'x (test-expr2)) 0 "Derivative of a constant")
   (check-equal? (diff 'x (test-expr3)) '(+ 1 0 1) "Derivative of a sum")
   (check-equal? (diff 'x (test-expr4)) '(+ (* x 1) (* x 1)) "Product rule")
   (check-equal? (diff 'x (test-expr5)) '(* 2 (expt x 1)) "Power rule 1")
   (check-equal? (diff 'x (test-expr6)) '(* n (expt x (- n 1))) "Power rule 2")
   (check-equal? (diff 'x (test-expr7))
                 '(+ (+ (* 3 (* 3 (expt x 2))) (* (expt x 3) 0)) (+ (* -2 1) (* x 0)) 0)
                 "Differentiation of a polinomy")))

# tests more complex rules
(define morecomplex-test
  (test-suite
   "Tests for more complex rules"
   (check-equal? (diff 'x (test-mc1)) '(exp x) "Derivative of e^x")
   (check-equal? (diff 'x (test-mc2)) '(/ 1 x) "Derivative of ln(x)")
   (check-equal? (diff 'x (test-mc3)) '(/ (* 2 (expt x 1)) (expt x 2)) "Chain rule 1")
   (check-equal? (diff 'x (test-mc4)) '(* (exp (expt x 2)) (* 2 (expt x 1))) "Chain rule 2")
   (check-equal? (diff 'x (test-mc5)) '(* 2 (* (expt (ln x) 1) (/ 1 x))) "Chain rule 3")))

# tests trig functions and other functions
(define morefunctions-test
  (test-suite
   "Tests for more functions"
   (check-equal? (diff 'x (test-mf1)) '(/ 1 (* x (ln 10))) "Derivative of logb(a)")
   (check-equal? (diff 'x (test-mf2)) '(* 1 (cos x)) "Derivative of sin")
   (check-equal? (diff 'x (test-mf3)) '(* (* -1 1) (sin x)) "Derivative of cos")
   (check-equal? (diff 'x (test-mf4)) '(* 1 (expt (sec x) 2)) "Derivative of tan")
   (check-equal? (diff 'x (test-mf5)) '(* 1 (* (sec x) (tan x))) "Derivative of sec")
   (check-equal? (diff 'x (test-mf6)) '(/ (- (* 1 (sin x)) (* (* 1 (cos x)) x)) (expt (sin x) 2)) "Derivative of x/sin(x)")   
   (check-equal? (diff 'x (test-mf7)) '(/ (* 0.5 1) (sqrt x)) "sqrt(x)")))

# tests simplying the output
(define simplify-test
  (test-suite
   "Simplify"
   (check-equal? (simplify (test-simplify-1)) 'x "Remove zeroes and sum numeric values")
   (check-equal? (simplify (test-simplify-2)) '(* 7 x) "Multiply numeric values")
   (check-equal? (simplify (test-simplify-3)) '(* 6 x) "Multiplication and expt")))

# runs tests
(run-tests basic-test)
(run-tests morecomplex-test)
(run-tests morefunctions-test)
(run-tests simplify-test)
