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

moveIn([A, B, C | T], NewState):-
    availableMovesIn([A, B, C | T], Moves),
    member(N, Moves),
    removeElementByIndexIn([A, B, C | T], N, NewState).

availableMovesIn(List, Output):-
    availableMovesIn(List, 0, Output).
availableMovesIn([], _, []).
availableMovesIn([_|T], N, [N | Output]):-
    Q is N+1,
    availableMovesIn(T, Q, Output).

removeElementByIndexIn([_|T], 0, T).

removeElementByIndexIn([H|T], N, [H | NewState]):-
    Q is N - 1,
    removeElementByIndexIn(T, Q, NewState).


:-use_module(library(lists)).

threeSumIn(Start,Goal, Output):-
    	getHeuristicIn(Start, H, Goal),
		pathIn([[Start,null, H]],[],Goal, Output).


pathIn(Open, Closed, Goal, [A, B, C]):-
		getBestChildIn(Open, [[A, B, C], Parent, H], RestOfOpen),
    	Sum is A + B + C,
    	Goal = Sum.

pathIn(Open, Closed, Goal, Output):-
		getBestChildIn(Open, Best, RestOfOpen),
		getchildrenIn(Best, RestOfOpen, Closed, Goal, Children),
		addListToOpenIn(Children , RestOfOpen, NewOpen),
		pathIn(NewOpen, [Best | Closed], Goal, Output).


getchildrenIn([State, Parent, _], Open ,Closed, Goal, Children):-
		bagof(X, movesIn(State, Open, Closed, Goal, X), Children), ! .
getchildrenIn(_,_,_,_, []):-!.


addListToOpenIn([], RestOfOpen, RestOfOpen).
addListToOpenIn([H|T], RestOfOpen, [H | NewOpen]):-
		addListToOpenIn(T, RestOfOpen, NewOpen).


removeFromOpenIn([State|RestOpen], State, RestOpen).
removeFromOpenIn([H|T], State, [H|RestOpen]):-
    removeFromOpenIn(T, State, RestOpen).


movesIn(State, Open, Closed, Goal, [Next, State, H]):-
		moveIn(State, Next),
    	getHeuristicIn(Next, H, Goal),
		\+ member([Next,_, _],Open),
		\+ member([Next,_, _],Closed).

calcSumIn([], 0).
calcSumIn([H|T], Sum):-
    calcSumIn(T, Sum1),
    Sum is H + Sum1.

getHeuristicIn([], Goal, Goal):-!.

getHeuristicIn(State, R, Goal):-
    calcSumIn(State, H),
	(   Goal > H -> R is Goal - H
    ;   R is H - Goal).

getBestChildIn([Child], Child, []):-!.
getBestChildIn(Open, Best, RestOpen):-
	getBestChild1In(Open, Best),
	removeFromOpenIn(Open, Best, RestOpen), !.

getBestChild1In([State], State):-!.
getBestChild1In([State|Rest], Best):-
	getBestChild1In(Rest, Temp),
	getBestIn(State, Temp, Best).

getBestIn([State, Parent, H1], [_, _, H2], [State, Parent, H1]):-
	H1 < H2, !.
getBestIn([_, _, _], [State1, Parent1, H1], [State1, Parent1, H1]).



% Problem 2 Informed


%Remove first occurrence
removeElementByValue([], _, []).
removeElementByValue([Elem|T], Elem, T):-!.
removeElementByValue([H|T], Elem, [H|Next]):-
    removeElementByValue(T, Elem, Next).

moveDeletiveEditing(State, NewState):-
    member(N, State),
    removeElementByValue(State, N, NewState).

%get difference in place


getHeuristic([],0,_):-!.

getHeuristic([H|T1],V,[H|T2]):-
    getHeuristic(T1,V,T2),!.

getHeuristic([_|T1],V,Goal):-
    getHeuristic(T1,Y,Goal),
    V is Y+1.

%Get Best Child
%State Representation --> [State, Parent, H]

getBest([State, Parent, H], [_, _, H1], [State, Parent, H]):-
	H < H1, !.
getBest([_, _, _], [State1, Parent1, H1], [State1, Parent1, H1]).

getBestChild1([State], State):-!.
getBestChild1([State | Rest], Best):-
    getBestChild1(Rest, Temp),
    getBest(State, Temp, Best).


getBestChild([Child], Child, []).
getBestChild(Open, Best, RestOpen):-
    getBestChild1(Open, Best),
    removeElementByValue(Open, Best, RestOpen).


%%%%%%DOVA
    %%case child not opened or closed

    % getBestChilds(Open,[State,P])
    %case child already open 
    %case child already closed 


%%%%%%%



getChildrenDeletive(State, Open ,Closed, Children, Goal):-
    bagof(X, movesDeletive( State, Open, Closed, X, Goal), Children).

getChildrenDeletive(_,_,_, [],_,_).

movesDeletive(State, Open, Closed, [Next,State,H], Goal):-
    moveDeletiveEditing(State, Next),
    \+ member([Next,_,_], Open),
    \+ member([Next,_,_], Closed),
    getHeuristic(Next, H, Goal).

% Get Path

pathDeletiveEditing(Open, Closed, Goal):-
	getBestChild(Open, [Goal, Parent, H], RestOfOpen),!.
    
pathDeletiveEditing(Open, Closed, Goal):-
    getBestChild(Open, [State,Parent , H], RestOpen),
    getChildrenDeletive(State, Open, Closed, Children, Goal),
    addListToOpenBFS(Children , RestOpen, NewOpen),
	pathDeletiveEditing(NewOpen, [[State,Parent , H] | Closed], Goal).
   
deletiveEditing(Start, Goal):-
    getHeuristic(Start, H, Goal),
    pathDeletiveEditing([[Start,null, H]],[], Goal), !.

addListToOpenBFS(Children, [], Children).
addListToOpenBFS(Children, [H|Open], [H|NewOpen]):-
		addListToOpenBFS(Children, Open, NewOpen).

printsolution([State, null, H],_):-!,
		write(State), write(' H:'), write(H), nl.
printsolution([State, Parent, H], Closed):-
		member([Parent, GrandParent, PC1, H1, TC1], Closed),
		printsolution([Parent, GrandParent, PC1, H1, TC1], Closed),
		write(State), write(' H:'), write(H),  nl.
%%problems might be moving or getting the best child or assigning heuristics
