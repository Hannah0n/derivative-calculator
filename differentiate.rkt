;; provide functions for export
(provide diff)
(provide simplify)

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
(define (make-log a b) (list 'log a b))
(define (make-sqrt a) (list 'sqrt a))
(define (make-cos a) (list 'cos a))
(define (make-sin a) (list 'sin a))
(define (make-tan a) (list 'tan a))
(define (make-sec a) (list 'sec a))

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

;; function for substracting differentiation
(define (diff-minus var expr)   ; a - b
  (let ((a (get-1st-param expr))
        (b (get-2nd-param expr)))
    (make-minus (diff var a) (diff var b))))

;; function for product differentiation
(define (diff-product var exp)
  (let ((e1 (get-1st-param exp))
        (e2 (get-2nd-param exp)))
    (make-sum (list (make-mul e1 (diff var e2)) (make-mul e2 (diff var e1))))))

;; function for quotient differentiation
(define (diff-div var exp)
  (let((f (get-1st-param exp))
       (g (get-2nd-param exp)))
    (make-div (make-minus (make-mul (diff var f) g) (make-mul (diff var g) f))
              (make-expt g 2))))

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

;; function for logb(x) differentiation
(define (diff-log var expr)
  (let ((a (get-1st-param expr))
        (b (get-2nd-param expr)))
       (make-div (diff var a) (make-mul var (make-ln b)))))

;; function for sqrt(x) differentiation
(define (diff-sqrt var expr)
  (let ((a (get-1st-param expr)))
       (make-div (make-mul 0.5 (diff var a)) (make-sqrt a))))

;; function for cos(x) differentiation
(define (diff-cos var expr)
  (let ((a (get-1st-param expr)))
       (make-mul (make-mul -1 (diff var a)) (make-sin a))))

;; function for sin(x) differentiation
(define (diff-sin var expr)
  (let ((a (get-1st-param expr)))
       (make-mul (diff var a) (make-cos a))))

;; function for tan(x) differentiation
(define (diff-tan var expr)
  (let ((a (get-1st-param expr)))
       (make-mul (diff var a) (make-expt (make-sec a) 2))))

;; function for sec(x) differentiation
(define (diff-sec var expr)
  (let ((a (get-1st-param expr)))
       (make-mul (diff var a) (make-mul (make-sec a) (make-tan a)))))

;; the dispatch table of supported operators to help the diff function
;; determine the appropriate sub-function to handle an expression
(define diff-dispatch
  (list (list '+ diff-sum)
        (list '* diff-product)
        (list '- diff-minus)
        (list '/ diff-div)
        (list 'expt diff-expt)
        (list 'exp diff-exp)
        (list 'ln diff-ln)
        (list 'log diff-log)
        (list 'sqrt diff-sqrt)
        (list 'cos diff-cos)
        (list 'sin diff-sin)
        (list 'tan diff-tan)
        (list 'sec diff-sec)))

;; is the given expression a multiplication?
(define (ismul? expr)
  (cond ((pair? expr) (eq? (get-op expr) '*))
        (#t #f)))

;; function to simplify multiplications
(define (simplify-mul expr)
  (let ((a (get-1st-param expr))
     (b (get-2nd-param expr)))
    (cond
      ((eq? a 1) (simplify b))  ; 1*b = b
      ((eq? b 1) (simplify a))  ; a*1 = a
      ((eq? a 0) 0)             ; 0*b = 0
      ((eq? b 0) 0)             ; a*0 = 0
      ((and (number? a) (number? b) (* a b)))  ; compute a*b
      ((and (not (number? a)) (number? b)) (simplify (make-mul b a))) ; put the constant to the left
        ; check if we have something like (* a (* b ... and multiply (a*b)
      ((and (number? a) (ismul? b) (number? (get-1st-param (simplify b))))
               (make-mul (* a (get-1st-param (simplify b))) (get-2nd-param (simplify b))))
      (#t (make-mul (simplify a) (simplify b))))))

;; removes all zeroes in a expression like (+ a1 a2 ... an)
(define (removeZeroes expr)
  (cond
     ((null? expr) expr)
     ((eq? (get-op expr) 0)
      (removeZeroes (params expr)))
     ((pair? (get-op expr))
      (cons (simplify (get-op expr)) (removeZeroes (params expr))))
     (#t (cons (get-op expr) (removeZeroes (params expr))))))

;; sums all the numbers in the expression (+ a1 a2 ... an)
(define (sumNumbers expr acum)
  (cond
    ((null? expr)
     (cond ((eq? acum 0) '())
           (#t (list acum))))
    ((number? (get-op expr))
     (sumNumbers (params expr) (+ acum (get-op expr))))
    ((pair? (get-op expr))
     (cons (simplify (get-op expr)) (sumNumbers (params expr) acum)))
    (#t (cons (get-op expr) (sumNumbers (params expr) acum)))))

;; for cases when we get (+) or (+ n)
(define (simplify-simple-sums expr)
  (cond
    ((pair? expr)
     (cond
       ((eq? (length expr) 1) 0)
       ((eq? (length expr) 2) (get-1st-param expr))
       (#t expr)))
    (#t expr)))

;; simplify a^b expressions
(define (simplify-expt expr)
  (let
    ((a (get-1st-param expr))
     (b (get-2nd-param expr)))
    (cond
      ((eq? b 0) 1)             ; a^0 = 1
      ((eq? b 1) (simplify a))  ; a^1 = a
      (#t (make-expt (simplify a) (simplify b))))))

;; flattens consecutive sums
(define (removeSubSums expr)
  (cond
    ((null? expr) expr)
    ((pair? (get-op expr)) (cond
         ((eq? (get-op (get-op expr)) '+)
          (append (params (removeSubSums (get-op expr))) (removeSubSums (params expr))))
         (#t   (cons (simplify (get-op expr)) (removeSubSums (params expr))))))
    (#t (cons (get-op expr) (removeSubSums (params expr))))))

;; handles sum simplification
(define (simplify-sum expr)
  (simplify-simple-sums (sumNumbers (removeZeroes (removeSubSums expr)) 0)))

;; main simplify function
(define (simplify expr)
  (cond ((pair? expr)   ; we need the expression to be complex in order to simplify it
         (let ((op (get-op expr)))  ; get the operator
           (cond
             ((eq? op '*) (simplify-mul expr))     ; special function for handling products
             ((eq? op '+) (simplify-sum expr))     ; special function for handling sums
             ((eq? op 'expt) (simplify-expt expr))  ; special function for handling a^b
             ((eq? (length expr) 2)
              (list (get-op expr) (simplify (get-1st-param expr)))) ; other operators with one parameter
             ((eq? (length expr) 3)          ; operators with two parameters
              (list (get-op expr) (simplify (get-1st-param expr)) (simplify (get-2nd-param expr)))))))
        (#t expr)))
        
