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

Sometimes, you can use an __accumulator__; an additional parameter to a function that accumulates the answer, to convert a non-tail-recursive function into a tail-recursive one.  

For example, the previous definition of factorial was not tail-recursive.  

Here is one that is, and makes use of an accumulator.  

```
(define (factorial n)
  (acc-factorial n 1))


  ;; auxiliary function that takes an additional parameter (the accumulator,
  ;; i.e. the result computed so far)
  (define (acc-factorial n sofar)
    (if (zero? n)
          sofar
	  (acc-factorial (- n 1) (* sofar n))))
```

__Simultaneous Recursion on Several Variables__ 

```
;; test whether two lists have the same length

(define (same-length x y)
  (cond ((and (null? x) (null? y)) #t)
          ((null? x) #f)
	          ((null? y) #f)
		          (else (same-length (cdr x) (cdr y)))))

;; do these two objects have the same shape?
(define (same-shape x y)
  (cond ((and (pair? x) (pair? y))
         (and (same-shape (car x) (car y))
              (same-shape (cdr x) (cdr y))))
  	((or (pair? x) (pair? y)) #f)
	(else #t)))
```

### Conditional Augmentation ###

Like augmenting recursion.  

Do not necessarily augment on every step. 

These functions are sometimes called *filtering* functions, because they remove elements from a list that do not pass some test.  

```
(define (func x)
  (cond (end-test end-value)
          (aug-test (augmenting-function augmenting-value (func reduced-x)))
          (else (func reduced-x))))

;; example:  remove all non-positive numbers from a list

(define (positive-numbers x)
  (cond ((null? x) '())
        ((> (car x) 0) (cons (car x) (positive-numbers (cdr x))))
        (else (positive-numbers (cdr x)))))
```

__Example: Insertion Sort__

```
;; variation on list-consing recursion
(define (insert x s)
   (cond ((null? s) (list x))
         ((< x (car s)) (cons x s))
	 (else (cons (car s) (insert x (cdr s))))))

;; augmenting recursion
(define (isort s)
  (if (null? s) '()
      (insert (car s) (isort (cdr s)))))
```

## Higher Order Functions ##

A *higher order function* is a procedure that either:  
* takes a procedure as an argument,  
* returns a procedure,  
* or both.  

A classic example of a higher order function is the *mapcar* function, which maps a procedure over a list.  

It is defined as follows:  

```
(define (mapcar a f)
  (cond ((null? a) ’())
        (else (cons (f (car a))
		     (mapcar (cdr a) f)))))
```

The Scheme standard does not define *mapcar*, but a more powerful procedure, called *map*.  

*mapcar* can only apply procedures of one argument to the members of a __single__ list.  

*map* eliminates this limitation; it can be applied to any number of lists.  

*Map* is a variadic procedure which accepts a number of lists, so the structure it ultimately processes is a list of lists.  
Like *mapcar*, it passes the car parts of these lists to its procedure argument.  
It then advances to the cdr parts of its list arguments.  

The Scheme standard also dictates that all lists passed to *map* must be of the same length, but there are some environments which violate the standard, and allow lists to be of unequal length.  

Recursion can be implemented using __lambda__ exclusively by using *self-application*.  

Example:  
```
(S (lambda (f x)
(cond ((zero? x) 1)
(else (* x (f f (- x 1))))))
5)

; => 120
```
This expression uses an *anonymous recursive procedure* to compute the factorial of 5. 

However, how can a procedure apply itself when it has no name? 

A procedure that is passed to S has to provide an additional argument which will be bound to a copy of itself.  

This is what the f argument of the lambda function above is used for.  

### Functions Inside a Data Structure ###

Store data in a data structure, e.g. an a-list.  

Data Directed Programming: When processing some data, use the data as a key to look up what function to use to process this data.  

__Data Structures in Scheme__  
In Scheme, lists and S-expressions are basic.  
Arrays can be simulated using lists, but access to elements "deep" in the list can be slow, since a list is a linked structure.  

__Vectors in Scheme__
Scheme provides a vector type that directly implements one dimensional arrays.  

Literals are of the form #(...).  
E.g. #(1 2 3).  

The function __(vector? val)__ tests whether val is a vector or not.  

The function __(vector v1 v2 ...)__ evaluates v1, v2, ... and puts them into a vector.  

Vectors are NOT lists, and lists are NOT vectors.  

What does __assoc__ do if more than one sublist with the same key exists?  
It returns the first sublist with a matching key.   
This property can be used to make a simple and fast function that updates association lists:  
```
(define
 (set-structure key alist val)
  (cons (list key val) alist)
  )
```

__Functions are First-Class Objects__ 
Functions may be passed as parameters, returned as a value of a function call, stored in objects, etc.  

This is a consequence of the fact that:  
```
(lambda (args) (body))
```

evaluates to a function, just as (+ 1 1) evaluates to an integer. 

__Scoping__
In Scheme, scoping is static [lexical].  

This means that non-local identifiers are bound to containing lambda parameters, or let values, or globally defined values.  

For example:  
```
(define (f x)
 (lambda (y) (+ x y)))
```

Function f takes one parameter, x.  
It returns a function (of y), with x in the returned function bound to the value of x used when f was called.  

Thus, 
```
(f 10) ≡ (lambda (y) (+ 10 y))
((f 10) 12) ⇒ 22
```
Unbound symbols are assumed to be global; there is a runtime error if an unbound global is referenced.  

We can use let bindings to create private local variables for functions.  

We can *encapsulate* internal state with a function by using private, let-bound variables.  

Let bindings can be subtle - must check to see if the let-bound value is created when the function is *created*, or when it is *called*.  

Using association lists and private bound values, we can encapsulate data and functions. 
This gives us the effect of class objects.  

```
(define (point x y)
 (list
  (list 'rect
   (lambda () (list x y)))
    (list 'polar
     (lambda ()
      (list
       (sqrt (+ (* x x) (* y y)))
        (atan (/ x y))))))
)
```
A call point(1 1) creates an association list of the form ((rect funct) (polar funct))  

__Building Data Structures__: Use __list__ in a constructor function.  

__Accessing Fields__: use __list-ref__ in a selector function.  

### Scheme Macros ###
Macros are said to be what makes Lisp, Lisp. 

Other languages, notably C and C++, have macros, but they are not like Lisp macros.  

In C, the macro preprocessor allows you to do limited substitution on snippets of C code.  
Sometimes, this produces working C code, while other times, it can result in invisible misplaced braces or semicolons that the compiler can't quite pinpoint.  

Lisp macros are full-blown Lisp procedures, with the power of any other Lisp procedure.  

Instead of text, they get lists that represent the bits of code that you want to change.  

The return value of a macro is a list representing the new program.  

Example of macros:   

Macro that emulates a while loop:  
```
(defmacro (while condition . body)
  `(let loop ()
       (cond (,condition
       	    (begin . ,body)
	    	    (loop)))))
```

Then you can write this program:  
```
(define counter 10)

(while (> counter 0)
    (display counter)
        (newline)
	    (set! counter (- counter 1)))
```

The while loop checks if the conditional expression is true, and if it is, it executes the body over and over again until the condition becomes false.  

__Hygenic Macros: Syntax-Rules__ 

Scheme introduces *hygenic* macros to the Lisp world.  

When writing such a macro, it is impossible to accidentally introduce variable names that the code you're altering can change.  

However, to write them, must abandon the concept of code being made out of simple lists.  

Syntax-rules macros rely on pattern-matching to define the valid ways to invoke the macro, and templates to define the output code.  

Example: while loop macro
```
(define-syntax while
  (syntax-rules ()
      ((_ condition . body)
           (let loop ()
	          (cond (condition
		  	      (begin . body)
			      	      (loop)))))))
```
If you refer to loop within the body if this version of while, it refers to a different loop from the one shown in the template.  
It is impossible to define a variable with syntax-rules that code from outside the macro can see.  
The form that says ( _ condition . body) is the pattern. _ is a wildcard.   
Anything can match it, and its value is not bound to anything. 
In this case, it is standing in the place where the word while goes.  
condition and body are pattern variables.  
They're not real variables.  
They won't have value at runtime, only within the template.  
Due to dotted-list notation, body is matched in the cdr position, which means it's all the forms that come after the body.  

__How Pattern Matching Works__
Implementing something like pattern-matching regular lists using syntax-rules requires two macros.  
One macro will match a single pattern, and evaluate an expression if it succeeds, or return a failure if it doesn't.  

