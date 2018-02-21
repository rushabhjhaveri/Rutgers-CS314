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
* Depending on the implementation, __eq?__ may or may not return __#t__ for primitive values such as numbers, strings, etc.  

### eqv? Equivalence Predicate ###

* Exactly same as the __eq?__ predicate, except that it will always return __#t__ for same primitive values. 
* Hence, __eqv?__ is a superset of __eq?__ and for most cases __eqv?__ should be used instead of __eq?__. 

### equal? Equivalence Predicate ###
* Exactly same as the __eqv?__ predicate, except that it can be used to test whether two lists, vectors, etc. have corrosponding elements which satisfy the __eqv?__ predicate. 


__In general__
1. Use the __=__ predicate when you wish to test whether two numbers are equivalent. 
2. Use the __eqv?__ predicate when you wish to test whether two non-numeric values are equivalent.  
3. Use the __equal?__ predicate when you wish to test whether two lists, vectors, etc. are equivalent.  
4. Don't use the __eq?__ predicate unless you know exactly what you're doing.  

## member ##
* Built into Scheme.
* __(member x list)__ evaluates to the part of the list which begins with the first occurrence of x, and evaluates to #f if x is not a member of list.  

## assoc ## 
* An *association list*, or *alist*, is a data structure used very frequently in Scheme. 
* An alist is a list of pairs, each of which is called an *association*. 
* The car of an association is called the *key*.  
* An advantage of the alist representation is that an alist can be incrementally augmented simply by adding new entries to the front.  
* If an alist is viewed as a mapping from keys to data, then the mapping can be not only augmented but also altered in a non-destructive   manner by adding new entries to the front of the alist.  

procedure: __alist? object__ : Returns #t if object is an association list (including the empty list); otherwise returns #f. Any object satisfying this predicate also satisfies list?. 

procedure: 
* __assq object alist  
* assv object alist  
* assoc object alist__

These procedures find the first pair in alist whose car field is object, and return that pair.  
The returned pair is always an element of alist, not one of the pairs from which alist is composed.  
If no pair in alist has object as its car, #f (n.b.: not the empty list) is returned. assq uses eq? to compare object with the car fields of the pairs in alist, while assv uses eqv? and assoc uses equal?.  

## let ##
Syntax:

```
(let ([id val-expr] ...) body ...+)

(let proc-id ([id init-expr] ...) body ...+)
```
The first form evaluates the val-exprs left-to-right, creates a new location for each id, and places the values into the locations.  
It then evaluates the bodys, in which the ids are bound. 
The last body expression is in tail position with respect to the let form. 
The ids must be distinct according to bound-identifier=?.  

The second form evaluates the init-exprs; 
the resulting values become arguments in an application of a procedure (lambda (id ...) body ...+), 
where proc-id is bound within the bodys to the procedure itself.  

## let * ##
Syntax 
```
(let* ([id val-expr] ...) body ...+)
```
Like let, but evaluates the val-exprs one by one, creating a location for each id as soon as the value is available. 
The ids are bound in the remaining val-exprs as well as the bodys, and the ids need not be distinct; later bindings shadow earlier bindings. 

## letrec ##

Syntax:
```
(letrec ([id val-expr] ...) body ...+)
```

Like let, including left-to-right evaluation of the val-exprs, but the locations for all ids are created first, 
all ids are bound in all val-exprs as well as the bodys, and each id is initialized immediately after the corresponding val-expr is evaluated. 
The ids must be distinct according to bound-identifier=?.  

__Note__: Referencing or assigning to an id before its initialization raises exn:fail:contract:variable. 
          If an id (i.e., the binding instance or id) has an 'undefined-error-name syntax property whose value is a symbol, 
          the symbol is used as the name of the variable for error reporting, instead of the symbolic form of id.  

