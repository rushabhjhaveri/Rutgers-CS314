# Scheme #

Scheme is a dynamic laguage, which blurs the line between runtime and compile-time.

Scheme is an interactive lamguage.

Read - Eval - Print - Loop [REPL]
* Scheme reads an expression into internal form.
* It then evaluates that expression.
* It then prints the result of that evaluation

Scheme is a __functional__ programming langauge.

In scheme, a program is an expression to be evaluated, __not__ a sequence of commands to be executed.

__Expression__: An expression in scheme can be 
* a literal constant [5, 3.1416, "hello"]
* a variable bound to some value [x, ?a, +)
* a funcion application

__Function Application__
* written as a list.
* parens around the list; white space separates elements.
* first element in the list specifies a function.
* the remaining elements specify the arguments.

__To evaluate a function application__
* Evaluate the first element.
* Evaluate the remaining elements.
* Apply the value of the first to the values of the rest.

This is like the mathematical concept of a function.

Function applications can be nested.

__Function Abstraction__

New functions are defined by a process called function abstraction.

* First, write an expression.
* Then, wrap it in a lambda form.
* This value of this lambda expression is a function with a formal parameter, and a body.
* You can use this expression-whose-value-is-a function just like you can use a variable-whose-value-is-a-function

__#t__ represents true.
__#f__ represents false.

## cond ##

Syntax: 
```
(cond (test1   
        (action 1))  
      (test2  
        (action2))  
      (test3  
        (action3))  
      (else  
        (action4)))  
```

Each test-action pair is enclosed in parentheses.

The __cond__ construct is akin to a sequence of __if__ tests.

In this example, test1 is just a variable reference, not a procedure call, i.e., we're testing to see if the value of the variable test1 is #f. 

If not, execute (action1), i.e., call the procedure action1. If it is false, control "falls through" to the next test, and keeps going until one of the tests evaluates to a true value (anything but #f).  

The actions corrosponding to a test are indented by one character. This lines the action up directly below the tests, rather than under the opening parentheses that group them together.

The __else__ clause of a cond is __optional__. If present, that branch is taken as the "default". If none of the other conditions evaluate to a true value, the __else__ branch is taken.

The __else__ clause is not really required, because we the same effect could be achieved by using a test expression that always evaluates to a true value. One way of doing this is to use the literal __#t__, the true boolean, because it's always true.

## not ##
```
not(v) --> boolean
```

Returns __#t__ if v is __#f__, and vice-versa

## or ##
Syntax
```
(or expr1 expr2 ... expr-n)   
```

return true if at least one of the expr's is true
... or more precisely, return expr-j if expr-j is the first expr that
evaluates to something other than #f.  Otherwise return #f.

## and ##
Syntax
```
(and <expression 1> ... <expression n>)
```
Expressions evaluated from left to right. 

The value of the first expression that returns false is returned. Any and all remaining expressions are not evaluated.

If all the expressions evaluate to __#t__ (true), then the value of the last expression is returned.

If there are no expressions, then __#t__ is returned.

## Lists ##

Elements enclosed within parentheses, each element separated by a whitespace.

External representation: (1 2 3 (4 5) 6) etc.

Internal representation: a singly linked-list.

Can have lists of lists, nested lists, et al.

## cons ##
Primitive procedure, means "construct".

cons takes two arguments [two lists / elements], and returns a list constructed from those two arguments.

Provides a means of combination for Scheme.

The two values joined with __cons__ are printed within parentheses, but with a dot printed in between them. 

```
  > (cons 1 2)
  (1 . 2)

  > (cons "banana" "split")
  ("banana" . "split")
  ```
  
The value returned by __cons__ is not *always* a list. In general, the result of __cons__ is a *pair*. 
  
The more traditional name for the cons? function is pair? 

## car ##

car -> *Contents of Address Part of Register*
   
Returns the first element of a list.

Note: car does not modify the original list; it just returns the first element of the list.

Syntax:

```
(car(lst))
```

## cdr ##
Syntax:

```
(cdr(lst))
```
cdr -> *Contents of Decrement Part of Register*

Returns a sublist consisting of all the elements of the original list, except the first.

## cadr ##

Nested application of __car__ and __cdr__.

```
(car (cdr (lst)))
```
The __cdr__ is evaluated first, returning a sublist containing all the elements except the first.
Then, __car__ is evaluated, returning the first element of the sublist, i.e., the second element of the original list.

Thus, __cadr__ returns the second item in a list.

__Similarly,__

* (cdadr x) means (cdr (car (cdr x)))
* (cdadr '(a (b c d) e)) => (c d)
* (cadadr x) means (car (cdr (car (cdr x)))) [But if you use it you are probably doing something wrong]
* (cdadadr x) is not defined

__A Few Examples__

* (cdr ‘((a) b (c d))) => (b (c d))
* (cadr ‘((a) b (c d))) => b
* (cddr ‘((a) b (c d))) => ((c d))
* (cdddr ‘((a) b (c d))) => ( )
* (cddddr ‘((a) b (c d))) => *error*

## quote ##

Syntax:

```
(quote datum)
```

__datum__ can be a symbol, boolean, number, string [character / byte], character, keyword, empty list, a pair, list, vector, hash table, etc.

__quote__ does not evaluate the argument passed; it just returns the argument and prints it on the console.

__Important Note__

```
'datum
```
is shorthand for 

```
(quote datum)
```

and this shorthand is almost always used instead of __quote__.

## lambda ##

A lambda expression creates [and returns] a function.

Syntax

```
(lambda (arg-id ...)
  body ...)
```

## define ##

Syntax

```
(define id expr)

(define (head args) body ...+)
```

The first form binds id to the result of expr.

The second form binds id to a procedure.

Values used for variables when not bound ("top level" values).

## Dynamic Typing ##

* Variables and fields don't have types; only values have types.
* A value is represented by a data structure with
  * a type code
  * a value
* Depending on the type, the value is either:
  * immediate data [e.g., integer]
  * a pointer to actual data on the heap [e.g., a pair]
  
## if ##
Syntax

```
(if test-expr then-expr else-expr)
```
If the test expression evaluates to true, then-expr is evaluated. Otherwise, else-expr is evaluated.

## Predicates ##

### = Equivalence Predicate ###

* Used to check whether two numbers are equal.
* If supplied with anything else but a number, it will raise an error.

### eq? Equivalence Predicate ###

* Used to check whether its two parameters represent the same object in memory.
* Note however that there's only one empty list '() in memory (actually the empty list doesn't exist in memory, but a pointer to the       memory location 0 is considered as the empty list). Hence when comparing empty lists eq? will always return #t (because they represent   the same object in memory)
* 
