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

