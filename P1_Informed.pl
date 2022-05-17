% Problem 1 Informed

move([A, B, C | T], NewState):-
    availableMoves([A, B, C | T], Moves),
    member(N, Moves),
    removeElementByIndex([A, B, C | T], N, NewState).

availableMoves(List, Output):-
    availableMoves(List, 0, Output).
availableMoves([], _, []).
availableMoves([_|T], N, [N | Output]):-
    Q is N+1,
    availableMoves(T, Q, Output).

removeElementByIndex([_|T], 0, T).

removeElementByIndex([H|T], N, [H | NewState]):-
    Q is N - 1,
    removeElementByIndex(T, Q, NewState).


:-use_module(library(lists)).

threeSum(Start,Goal, Output):-
    	getHeuristic(Start, H, Goal),
		path([[Start,null, H]],[],Goal, Output).


path([[[A, B, C], _, _] | _], _, Goal, [A, B, C]):-
        Sum is A + B + C,
        Goal = Sum.

path(Open, Closed, Goal, Output):-
		getBestChild(Open, Best, RestOfOpen),
		getchildren(Best, RestOfOpen, Closed, Goal, Children),
		addListToOpen(Children , RestOfOpen, NewOpen),
		path(NewOpen, [Best | Closed], Goal, Output).


getchildren([State, Parent, _], Open ,Closed, Goal, Children):-
		bagof(X, moves(State, Open, Closed, Goal, X), Children), ! .
getchildren(_,_,_,_, []).


addListToOpen([], RestOfOpen, RestOfOpen).
addListToOpen([H|T], RestOfOpen, [H | NewOpen]):-
		addListToOpen(T, RestOfOpen, NewOpen).


removeFromOpen([State|RestOpen], State, RestOpen).
removeFromOpen([H|T], State, [H|RestOpen]):-
    removeFromOpen(T, State, RestOpen).


moves( State, Open, Closed, Goal, [Next, State, H]):-
		move(State, Next),
    	getHeuristic(Next, H, Goal),
		\+ member([Next,_, _],Open),
		\+ member([Next,_, _],Closed).

getHeuristic([], 0, Goal):-!.

getHeuristic([H|T],R,Goal):-
	getHeuristic(T,TR, Goal),
	Tmp is Goal - H,
    R is Tmp + TR.

getBestChild([Child], Child, []).
getBestChild(Open, Best, RestOpen):-
	getBestChild1(Open, Best),
	removeFromOpen(Open, Best, RestOpen).

getBestChild1([State], State):-!.
getBestChild1([State|Rest], Best):-
	getBestChild1(Rest, Temp),
	getBest(State, Temp, Best).

getBest([State, Parent, H1], [_, _, H2], [State, Parent, H1]):-
	H1 < H2, !.
getBest([_, _, _], [State1, Parent1, H1], [State1, Parent1, H1]).

