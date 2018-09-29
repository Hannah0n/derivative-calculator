# derivative-calculator
A program to differentiating simple expressions by applying the appropriate differentiating rules such as differentiation of sums, product rule, power rule, chain rule, and quotient rule.


Expressions to diffrentiate contain
```
* constants
* variables
* sums with an arbitrary number of terms
* products with two terms
* exponents
* log
* sqrt
* quotients
* trig functions (cos, sin, tan, sec)
```


### Examples
Expressions are represented in list format using Racket's standard prefix notation. That is, a formula such as  ![ex formula](http://latex.codecogs.com/gif.latex?3x&plus;4y&plus;6x%5E%7B3%7D)  is represented as	(+ (* 3 x) (* 4 y) (* 6 (expt x 3))).

```
(diff 'x '4)			=>  0

(diff 'x '(* 2 x))	 	=> (+ (* 0 (* x)) (* 2 1)) 	   (i.e. 2)
    
(diff 'y '(* 2 y))		=> (+ (* 0 (* y)) (* 2 1)) 	   (i.e. 2)

(diff 'x '(+ x (* x x)))	=> (+ 1 (+ (* 1 (* x)) (* x 1)))   (i.e. 1 + x + x)

(diff 'x '(expt x 4))		=> (* 4 (expt x 3))		   (i.e. 4 * x3)
```
