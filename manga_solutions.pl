move([A, B, C | T], NewState):-
    availableMoves([A, B, C | T], Moves),
    member(N, Moves),
    removeElement([A, B, C | T], N, NewState).

availableMoves(List, Output):-
    availableMoves(List, 0, Output).
availableMoves([], _, []).
availableMoves([_|T], N, [N | Output]):-
    Q is N+1,
    availableMoves(T, Q, Output).

removeElement([_|T], 0, T).

removeElement([H|T], N, [H | NewState]):-
    Q is N - 1,
    removeElement(T, Q, NewState).


:-use_module(library(lists)).

threeSum(Start,Goal, Output):-
		path([[Start,null]],[],Goal, Output).


path([[[A, B, C],_] | _], _, Goal, [A, B, C]):-
        Sum is A + B + C,
        Goal = Sum.

path(Open, Closed, Goal, Output):-
		removeFromOpen(Open, [State, Parent], RestOfOpen),
		getchildren(State, Open, Closed, Children),
		addListToOpen(Children , RestOfOpen, NewOpen),
		path(NewOpen, [[State, Parent] | Closed], Goal, Output).


getchildren(State, Open ,Closed , Children):-
		bagof(X, moves( State, Open, Closed, X), Children), ! .
getchildren(_,_,_, []).


addListToOpen([], RestOfOpen, RestOfOpen).
addListToOpen([H|T], RestOfOpen, [H | NewOpen]):-
		addListToOpen(T, RestOfOpen, NewOpen).


removeFromOpen([State|RestOpen], State, RestOpen).


moves( State, Open, Closed,[Next,State]):-
		move(State, Next),
		\+ member([Next,_],Open),
		\+ member([Next,_],Closed).

% Problem 1 Informed

% Problem 2 Informed