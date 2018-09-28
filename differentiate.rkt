;; provide functions for export
(provide diff)

;; is x an atom (not a list)?
(define (atom? x) (not (pair? x)))

;; functions to make different expressions
(define (make-sum lst) (cons '+ lst))
(define (make-mul a b) (list '* a b))
(define (make-div a b) (list '/ a b))
(define (make-minus a b) (list '- a b))
(define (make-expt a b) (list 'expt a b))
(define (make-exp a) (list 'expt a))
(define (make-ln a) (list 'ln a))

;; functions for accesing different parts of an expression
(define (get-op expr) (car expr))
(define (get-1st-param expr) (cadr expr))
(define (get-2nd-param expr) (caddr expr))
(define (params expr) (cdr expr))
(define (op-name lst) (caar lst))
(define (op-fn lst) (cadar lst))

;; main differentiation function
(define (diff var expr)
  (cond ((number? expr) 0)  ; numeric constant 
        ((eq? expr var) 1)  ; variable
        ((atom? expr) 0)    ; symbolic constant
        (#t                 ; the expression is not simple
         (let ((op (get-op expr)))
           (letrec ([f (lambda (var expr lst)  ; function for dispatching the correct function
                         (cond
                           ((null? lst) (list 'functionNotSupported: op))
                           ((eq? (op-name lst) op) ((op-fn lst) var expr))
                           (#t (f var expr (cdr lst)))))])
             (f var expr diff-dispatch))))))

;; function for sum differentiation
(define (diff-sum var exp)
  (cons '+ (map (lambda (exp) (diff var exp))
       (params exp))))

;; function for product differentiation
(define (diff-product var exp)
  (let ((e1 (get-1st-param exp))
        (e2 (get-2nd-param exp)))
       (make-sum (list (make-mul e1 (diff var e2)) (make-mul e2 (diff var e1))))))

;; function for e^f(x) differentiation
(define (diff-exp var expr)
  (let ((e1 (get-1st-param expr)))
    (cond
      ((eq? e1 var) expr)
      (#t (make-mul expr (diff var e1))))))

;; function for ln(x) differentiation
(define (diff-ln var expr)
  (let ((e1 (get-1st-param expr)))
       (make-div (diff var e1) e1)))

;; decrements the expression. If the expression is a number it computes the result
(define (decrement exp)
  (cond
    ((number? exp) (- exp 1))
    (#t (make-minus exp 1))))

;; function for f(x)^n differentiation
(define (diff-expt var expr)
  (let ((base (get-1st-param expr))
        (expo (get-2nd-param expr)))
      (cond
        ((eq? base var) (make-mul expo (make-expt base (decrement expo))))  ;; x^number
        (#t                               ;; apply chain rule
         (make-mul expo (make-mul (make-expt base (decrement expo)) (diff var base)))))))

;; the dispatch table of supported operators to help the diff function
;; determine the appropriate sub-function to handle an expression
(define diff-dispatch
  (list (list '+ diff-sum)
        (list '* diff-product)
        (list 'expt diff-expt)
        (list 'exp diff-exp)
        (list 'ln diff-ln)))
