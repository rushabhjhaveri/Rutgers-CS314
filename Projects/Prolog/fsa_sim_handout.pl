%%%% project 2, CS 314, Spring, 2018.  Prof. Steinberg 

%%%% simulates a non-deterministic finite state
%%%% automaton.

%%% ================ representations

%%% input character string is represented as a list of symbols, e.g., [a, b, a, c]

%%% A state is represented using the functor state, whuch takes 3 arguments:
%%%   - name, a symbol: the name of this state
%%%   - transition list: a list of transition structures
%%%   - accepting:  the symbol yes or no

%%% a transition structure is represented using the functor transition,
%%% which takes 2 arguments:
%%%  - character, which is a character or epsilon
%%%  - name of resulting state.

%%% There is no special structure or functor for an fsa.  It is just represented as a list of state structures, the
%%% first state structure in the list is the starting state of the fsa

%%% e.g., this fsa:

%   ___  0			 ___   0
%  |   |	 	   1 	|   |
%  \->  even <----------------odd <-/
%           \   	      ^	 |    
%      	     \----------------|  | 1
%	   1                     V 
% State even is the		 foo ------------> bar
%   start state                        epsilon
% Even and bar are
%   accepting states.
%
%%% is represented as follows:
%%%   [state(even, [transition(0, even), transition(1, odd)], yes),
%%%    state(odd,[transition(1, foo), transition(0, odd), transition(1, even)], no),
%%%    state(foo, [transition(epsilon, bar)], no)
%%%    state(bar, [ ], yes)]

%%% ================ predicates

%%% state_struct(+Name,  +States, -State_struct)
%%%   Name is a state name,
%%%   States a list of state structures, and
%%%   State_Struct the state structure from States with name Name

% define state_struct here

/*
   If the state structure is a member of the list of state structures States, 
   Then state_struct will return said member with name Name.
*/
state_struct(Name, States, state(Name, TransitionList, Accepts)) :- 
	member(state(Name, TransitionList, Accepts), States).

%%% next_state_name(+State_struct, +Characters, -Next_name, -Next_chars)
%%% find (the name of) a next state to transition to 
%%%   State_struct is the current state,
%%%   Characters is the list of characters currently left in the input,
%%%   Next_name is the name of the next state, and Next_chars is the
%%%     remaining list of input chars

/*
If at the current state: 
 - have the transition list of current state
   
List of input chars - divide into [Head | Tail] = [First_Character | Rest_of_the_List]
   
Next_chars gets bound to Rest_of_the_List 

Next_name becomes transition from current state to state wherein the char == First_Character

Lets say input chars is [a,b,c] and we are in state S1.

So would go through the transition list in S1 to try to find which state to go next. 

If you find transition for a, then you would “return” the name of the next state and the rest of the input char [b,c] to your accept predicate. 

Now, if you find an epsilon in your transition list, you would do the same thing. 

You “return” the name of the next state that the epsilon goes to, but since epsilon does not consume any input, you would return the original char list [a,b,c].
*/


% define next_state_name here
% next_state_name(state(_,[transition(epsilon, NextName) | _], _), Chars, NextName, Chars). 
next_state_name(state(_,TransitionList,_), Chars, NextName, Chars) :- 
	member(transition(epsilon, NextName), TransitionList).

next_state_name(state(_,TransitionList,_), [FirstChar | Rest], NextName, Rest) :- 
	member(transition(FirstChar, NextName), TransitionList).

% accepts(+State_struct, +Fsa, +Chars) true if Fsa accepts character
% string Chars when starting from the state represented by State_struct

% define accepts here
accepts(state(_,_,yes), _, []).

accepts(State_struct, FSA, Chars) :- 
	next_state_name(State_struct, Chars, NextName, Rest),
	state_struct(NextName, FSA, NextState),
	accepts(NextState, FSA, Rest).

%%% run(Fsa, Chars) Fsa is a list of state stuctures representing
%%% a Fsa (so that the first state in the list is the start state of
%%% the fsa), Chars is a list of symbols and numbers representing
%%% the input to the fsa.  run succeeds if and only if the fsa
%%% would accept the sequence of characters

run([State1 | States], Chars):- accepts(State1,[State1 | States], Chars).

% demo1 and demo2 demonstrate calls to run.  They each specify a
% automaton and allow you to run that automaton with different input
% strings, by calling, e.g., demo1([1, 0]).
%

% demo1 and demo2 are here to show you how to call your code.  They
% are NOT intended to provide sufficient test data for your code.  Among
% other things, they do not test epsilon transitions.
% demo1 shows a deterministic finite state automaton.
% demo1([ ]) should return true, as should demo1([1,1]), demo1([0,1,1]),
%     demo1([1,1,0,0,1,0,1]), etc (any string with an even number of 1's)
%     demo1([1]) should return false, as should demo1([1,0,1,1]) as should any
%     string with an odd number of 1's
demo1(Chars):- run([state(even,[ transition(0, even),transition(1, odd)], yes),
                    state(odd,[transition(0, odd), transition(1, even)], no)],
		    		   Chars).

% demo2 shows non-determinism:  there are transitions from state odd
%    both to even and to other for the character 1.  both demo2([1,1,0]). and demo2([1,1,2]).
%    should return true.
demo2(Chars):- run([state(even,[ transition(0, even),transition(1, odd)], yes),
               state(odd,[transition(1, even), transition(1, other), transition(0, odd)], no),
               state(other,[transition(2, otherb)],no),
               state(otherb,[  ], yes)],
  	       Chars).

/*
demo3(Chars):-run([state(a, [transition(0, a), transition(1,b)], no),
              state(b, [transition(0,c)], no),
              state(c, [transition(0, d)], yes),
              state(d, [transition(1,c)],no)],
              Chars).


demo4(Chars):-run([state(a, [transition(1, a), transition(2,a),transition(0,b),transition(1,b)], no),
              state(b, [transition(1,b), transition(2,b), transition(epsilon, c),transition(epsilon, d)], no),
              state(c, [], no),
              state(d, [transition(0,c),transition(3,c), transition(3, e)],yes),
              state(e, [transition(2, c), transition(epsilon, a), transition(1,d)],yes)],
              Chars).

demo5(Chars):-run([state(a, [transition(1, b), transition(epsilon, c)],no),
              state(b, [transition(2,c)], no),
              state(c, [], yes)],
              Chars).

demo6(Chars):-run([state(a, [transition(1, a), transition(epsilon, b)], no),
              state(b, [transition(1, c), transition(epsilon,c)], no),
              state(c, [transition(epsilon, d)], no),
              state(d, [transition(2,e)], no),
              state(e, [transition(1,c), transition(1,a)], yes)],
              Chars).

demo7(Chars):-run([state(a, [transition(1, b), transition(2,a),transition(3,a),transition(1,c),transition(epsilon,c)], no),
              state(b, [transition(1,d), transition(2,f), transition(1, f)], no),
              state(c, [transition(0, a), transition(2,c), transition(3,c), transition(epsilon,e)], no),
              state(d, [transition(epsilon,f),transition(0,d)],no),
              state(e, [], no),
              state(f,[transition(0, a), transition(1,e)], yes)],
              Chars).

demo8(Chars):-run([state(a, [transition(4,c), transition(4,b), transition(5,b), transition(6,d)],no),
              state(b, [transition(6,c)], no),
              state(c, [transition(epsilon,e)], yes),
              state(d, [transition(6,f)], no),
              state(e, [transition(5,c), transition(3,e)],no),
              state(f, [transition(5,f)], yes)],
              Chars).

*/
