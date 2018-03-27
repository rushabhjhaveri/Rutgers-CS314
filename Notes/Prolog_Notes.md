# Prolog # 

Prolog [*pro*gramming in *log*ic] is one of the most widely used programming languages in AI research.  

As opposed to imperative lamguages such as C or Java, Prolog is a *declarative* programming language.  

This means that when implementing the solution to a problem, instead of specifying *how* to achieve a certain goal in a certain situation, we specify *what* the situation[*rules* and *facts*] and the goal [*query*] are, and let the Prolog interpreter derive the solution for us.  

Prolog is very useful in some problem areas such as: 
* AI 
* Natural Language Processing 
* Databases  

Using Prolog programs means asking Prolog questions about the previously described world.  
The simplest way of describing the world is by stating *facts*. 
E.g. of a fact:  
```
bigger(elephant, horse).
```

In mathematical terms, the bigger-relation in the example given above is transitive.  

However, this has not been defined in the program.  

The correct interpretation of the negative answer given by Prolog to bigger(elephant, monkey) [even though we know elephants are bigger than monkeys] is: from the information communicated to the system, *it cannot be proved that an elephant is bigger than a monkey*.  

To get a positive response, we need to provide a more accurate description of the world.  

One way of doing this would be to add the remaining facts. However, this is clearly too much work and effort, and not a smart and efficient way to do this.  

The more efficient solution would be to define a new relation [e.g., is_bigger], which is a transitive closure of bigger.  

In Prolog, such statements are called *rules*, and are implemented as follows:  
```
Head :- Body.
```
e.g., 
```
is_bigger(X, Y) :- bigger(X, Y).
is_bigger(X, Y) :- bigger(X, Z), is_bigger(Z, Y).
```
In these rules, 
* :- means something like "if"
* the comma [,] between bigger(X,Z) and is_bigger(Z,Y) stands for "and"
* X, Y, Z are variables, indicated by the use of uppercase letters.  

[If] Prolog cannot find the fact in its database, it tries to use [available] rules instead.  

This is done by *matching* the query with the head of the rule.  
When doing so, variables get instantiated.  

To prove the *goal*, Prolog may have to prove any number of *subgoals*, again with the same variable instantiations.  
This process is repeated recursively until the facts that make up the chain between the two intended goals are found and the query finally succeeds.  

## Prolog Syntax ## 

### Terms ###
The central data structure in Prolog is a *term*.  

There are four kinds of terms: 
* atoms 
* numbers 
* variables 
* compound terms.  

Atoms and numbers are sometimes grouped together and called *atomic terms*.  

__Atoms__: Atoms are generally strings made up of lower and uppercase letters, digits, and the underscore, *starting with a lowercase letter*.  
Examples of valid atoms: elephant, b, abcXYZ, x_123  

A series of arbitrary characters enclosed in single quotes also denotes an atom.  
e.g. : 'This is a Prolog atom'  

Strings made up of SOLELY of special characters like + - * = < > : & are also atoms.  

__Numbers__: All Prolog implementations have an integer type; a sequence of digits, optionally preceded by a minus [-]. Some implementations also support floating-point integers.  

__Variables__: Variables are strings of letters, digits, and the underscore, *starting with an uppercase letter or an underscore*.  
Examples of valid variables: X, Elephant, \_4711, X_1_2, MyVar, \_.  

The single underscore constitutes a special case; it is called an *anonymous variable*.  
An anonymous variable is used when the value of a variable is of no particular interest.  
Multiple occurrences of the anonymous variable in one expression are assumed to be distinct, i.e., their values do not necessarily have to be the same.  

__Compound Terms__: Compound terms are made up of a *functor* [a Prolog atom] and a number of *arguments* [Prolog terms, i.e., atoms, numbers, variables, or other compound terms] enclosed in parentheses and separated by commas.  

Examples of compound terms: 
is_bigger(horse, X), f(g(X, _ ), 7), 'My Functor'(dog)  

It is important to not put any blank characters between the functor and the opening parentheses, otherwise Prolog will not understand what you are trying to say.  

In other places, spaces can be very helpful for making programs more readable.  

A set of compound terms and atoms put together form the set of Prolog *predicates*.  

A term that does not contain any variables is called a *ground term*.  

### Clauses, Programs, and Queries ### 
Facts and rules are also called *clauses*.  

__Facts__: A fact is a predicate followed by a full-stop.  
Examples: bigger(whale, _ ).  
life_is_beautiful.  

Intuitive meaning of a fact: define a certain instance of a relation as being true.  

__Rules__: A rule consists of a *head* [a predicate], and a *body*.  

The head and body are separated by the *:-* sign.  

Like every Prolog expression, a rule must be terminated by a full-stop.  

Examples:  
```
is_smaller(X, Y) :- is_bigger(Y, X).
aunt(Aunt, Child) :-
	sister(Aunt, Parent),
	parent(Parent, Child).
```

Intuitive meaning of a rule is that the goal expressed by its head is true if the Prolog system can show that all of the expressions [subgoals] in the rule's body are true.  

__Programs__: A Prolog program is a sequence of clauses.  

__Queries__: After compilation, a Prolog program is run by submitting queries to the interpretor.  
A query has the same structure as the body of a rule; i.e., it is a sequence of predicates separated by commas and terminated by a full-stop.  
Queries can be entered at the Prolog prompt, which in most implementations, looks like:  
```
?-.
```
Examples:  
```
?- is_bigger(elephant, donkey).
?- small(X), green(X), slimy(X).
```

Intuitively, when submitting a query, we are asking Prolog whether all its predicates are true.  

### Built-In Predicates ### 

Built-ins can be used in a similar way as user-defined predicates.  
The important difference between the two is that a built-in predicate is not allowed to appear as the principal functor in a fact or the head of a rule.  
This must be so, because using them in such a position would effectively mean changing their definition.  

__Equality__: Maybe the most important built-in predicate is = (equality).  
Instead of writing expressions such as =(X, Y), we usually write more conveniently X = Y.  
Such a goal succeeds, if the terms X and Y can be matched.  

__Guaranteed Success and Certain Failure__: Sometimes it can be useful to have predicates that are known to either fail or succeed in any case.  
The predicates fail and true serve exactly this purpose.  
Some Prolog systems also provide the predicate false, with exactly the same functionality as fail.  

__Consulting Program Files__: Program files can be compiled using the predicate consult.  
The argument has to be a Prolog atom denoting the program file you want to compile.  
For example, to compile the file big-animals.pl submit the following query to Prolog:  
```
?-consult('big-animals.pl').
```
If the compilation is successful, Prolog will reply with Yes.  
Otherwise a list of errors will be displayed.  

__Output__: If besides Prolog’s replies to queries you wish your program to have further output you can use the write/1 predicate.  
The argument can be any valid Prolog term.  
In the case of a variable its value will get printed to the screen.  
Execution of the predicate nl/0 causes the system to skip a line.  

__Checking the Type of a Prolog Term__: 

__Help__: Most Prolog systems also provide a help function in the shape of a predicate, usually called help.  
Applied to a term (like the name of a built-in predicate) the system will display a short description, if available.  

## Answering Queries ##

### Matching ### 
Two terms are said to match if they are either identical or if they can be made identical by means of variable instantiation.  

Instantiating a variable means assigning it a fixed value.  

Two free variables also match, because they could be instantiated with the same ground term.  

It is important to note that the same variable has to be instantiated with the same value throughout an expression.  

The only exception to this rule is the anonymous variable _ , which is considered to be unique whenever it occurs.  

### Goal Execution ### 

Submitting a query means asking Prolog to try to prove that the statement(s) implied by the query can be made true provided the right variable instantiations are made.  

The search for such a proof is usually referred to as goal execution.  

Each predicate in the query constitutes a (sub)goal, which Prolog tries to satisfy one after the other.  

If variables are shared between several subgoals their instantiations have to be the same throughout the entire expression.  

If a goal matches with the head of a rule, the respective variable instantiations are made inside the rule’s body, which then becomes the new goal to be satisfied.  

If the body consists of several predicates the goal is again split into subgoals to be executed in turn.  

In other words, the head of a rule is considered provably true, if the conjunction of all its body-predicates are provably true.  

If a goal matches with a fact in our program, the proof for that goal is complete and the variable instantiations made during matching are communicated back to the surface.  

Note that the order in which facts and rules appear in our program is important here.  

Prolog will always try to match its current goal with the first possible fact or rule-head it can find.  

If the principal functor of a goal is a built-in predicate the associated action is executed whilst the goal is being satisfied.  

Sometimes there is more than one way of satisfying the current goal.  

Prolog chooses the first possibility (as determined by the order of clauses in a program), but the fact that there are alternatives is recorded.  

If at some point Prolog fails to prove a certain subgoal, the system can go back and try an alternative way of executing the previous goal.  

This process is known as backtracking.  

## Recursion ## 

[Refer to this.](http://www.doc.gold.ac.uk/~mas02gw/prolog_tutorial/prologpages/recursion.html#ex8)  


