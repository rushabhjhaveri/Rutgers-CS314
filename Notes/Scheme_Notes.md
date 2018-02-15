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

__cond__

