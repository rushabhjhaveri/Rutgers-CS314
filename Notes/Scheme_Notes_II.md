## Scheme Recursion ## 
### Augmenting Recursion ### 
Many recursive functions build up their answer bit by bit. 
These are called __augmenting recursive functions__.  
```
;; general form

(define (func x)
  (if end-test
      end-value
      (augmenting-function augmenting-value (func reduced-x))))
``` 

__Classic example: the Factorial__ 
```
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))
```
Many Scheme functions operate on lists, so we often see an important special case of augmenting recursion where our augmenting function is cons and our base case is the empty list.  

Notice that because we are using cons, we are actually constructing a brand new list and returning it.  

In other words, we are not changing the original list.  

A final important kind of recursion over lists is the class of reducing functions, because they reduce a list of elements to a single element. 
```
(define (double-each s)
  (if (null? s) 
      '()
      (cons (* 2 (car s)) (double-each (cdr s)))))
```
Our base value (the value returned when we reach the base case) isn't always ():  
```
(define (my-append x y)
  (if (null? x) y
      (cons (car x) (my-append (cdr x) y))))
```
A final important kind of recursion over lists is the class of reducing functions, because they reduce a list of elements to a single element.  
```
;; sum the elements of a list

(define (sumlist x)
  (if (null? x) 0
      (+ (car x) (sumlist (cdr x)))))
``` 
### Tail Recursion ### 
In tail recursion, we don't build up a solution, but rather, just return a recursive call on a smaller version of the problem.  

Double-test tail recursion is the most common form:  
```
;; general form:

(define (func x)
  (cond (end-test-1 end-value-1)
        (end-test-2 end-value-2)
        (else (func reduced-x))))
```

__Example__
```
;; example: are all elements of a list positive?

(define (all-positive x)
  (cond ((null? x) #t)
        ((<= (car x) 0) #f)
        (else (all-positive (cdr x)))))
;; (all-positive '(3 5 6))  => #t
;; (all-positive '(3 5 -6))  => #f
```

Some tail recursive functions will have more than one argument.  

All but one of the arguments are passed along unaltered.  

__Example__  
```
;; example: member:

(define (my-member e x)
  (cond ((null? x) #f)
        ((equal? e (car x)) #t)
        (else (my-member e (cdr x)))))
```

A less commonly seen form is single-test tail recursion.  

In particular, tail recursive functions don't use stack space for every recursive call.  

### Accumulator Recursion ### 
__Using Accumulators to Make a Function Tail-Recursive__ 
