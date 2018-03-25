% Author: Rushabh Jhaveri 
% NetID: rrj28 

% ----------------------------------------------------------------------------------------------
% [III]
% Translate the following into a set of prolog facts and rules. 
% It should be possible for Prolog to infer E from A-D.

% A. Joe is in the toy department.
in_dept(joe, toy_dept). 

% B. If someone is in a department, they report to the head of the department.
% First check if X is in the department. If he/she is, they report yo the head of the department.           
report_to(X, Y, dept):- in_dept(X, dept), head_of_dept(Y, dept).

% C. Sam is the head of the toy department. 
head_of_dept(sam, toy_dept).

% D. Everyone's salary is less than the salary of the person they report to.
is_paid_less(X, Y):- report_to(X, Y, _).

% E. Joe's salary is less than Sam's salary. 
% is_paid_less(_Joe, _Sam).

% -----------------------------------------------------------------------------------------------

% [IV]
% Write a definition in Prolog for the predicate trib(N, T) which is true if T is the Nth Tribonacci number.

% Where X,Y,Z are the three first elements of the Tribonacci sequence:

% Prototype: addThis(N, X, Y, Z, NthElement)

addThis(0, X,_Y,_Z,X). % returns the 0th element
addThis(1,_X, Y,_Z,Y). % returns the 1st element
addThis(2,_X,_Y, Z,Z). % returns the 2nd element

% returns the sum of the three elements
% i.e. the fourth element
addThis(3,X,Y,Z,Next):-
	    Next is X+Y+Z.

% returns the Nth element recursively
addThis(N,X,Y,Z,Next):-
	N > 3,
	NewN is N-1,
	W is X+Y+Z,
	addThis(NewN,Y,Z,W,Next).

trib(N,Goal):- addThis(N,0,0,1,Goal).

% -----------------------------------------------------------------------------------------------

% [V] 
% Write a definition in Prolog for the predicate echo, where echo(A, B) is true for lists A and B if B has the same elements as A, but doubled. 
% E.g. echo([dog, cat], [dog, dog, cat, cat]) is true, but echo([dog, cat], [dog, cat, dog, cat]) is not true. 
% echo([dog, cat], L) should bind L to [dog, dog, cat, cat].
echo([],[]).
echo([I|R],[I,I|RD]) :-
	    echo(R,RD).
% -----------------------------------------------------------------------------------------------

% [VI]
% Write a definition in Prolog for the predicate supressEchos(A, B) is true if B is A with consecutive duplicates removed.
% E.g., suppressEchos( [dog, cat, cat, cat, moose, moose, cat] , [dog, cat, moose, cat]).
% is true,
% and suppressEchos([dog, dog, cat, cow, cow, cow], B). binds B to [dog, cat, cow].

% Empty list is an empty list with duplicates removed.
suppressEchos([], []).

% Single element list is itself with duplicates removed. 
suppressEchos([X], [X]).

% The result of removing duplicates from [X, X|T] should be the same as the result of removing duplicates from [X|T].
suppressEchos([X, X|T], [X|R]):- suppressEchos([X|T], [X|R]).

% The result of removing duplicates from [X, Y|T], where X and Y are different, 
% Should be the result [X|R], where R is the result of removing duplicates from [Y|T].
suppressEchos([X,Y|T], [X|R]):-
	X \== Y, 
	suppressEchos([Y|T], R).
