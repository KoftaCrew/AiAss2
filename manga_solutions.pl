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


getHeuristic([H|T1],N,Goal):-
    not(member(H, Goal)),
    getHeuristic(T1,V, Goal),
    N is V + 1.

getHeuristic([H|T1],N,Goal):-
    member(H, Goal),
    getHeuristic(T1,N, Goal). 
    
getHeuristic([], 0, _). 

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
    removeElementByValue(Best, Open, RestOpen).



%%%%%%DOVA
    %%case child not opened or closed

    % getBestChilds(Open,[State,P])
    %case child already open 
    %case child already closed 


%%%%%%%



getChildrenDeletive(State, Open ,Closed, Children, H, Goal):-
    bagof(X, movesDeletive( State, Open, Closed, X, H, Goal), Children).
getChildrenDeletive(_,_,_, [],_,_).

movesDeletive(State, Open, Closed, [Next,State,H], H, Goal):-
    moveDeletiveEditing(State, Next),
    \+ member(State, Open),
    \+ member(State, Closed),
    getHeuristic(Next, H, Goal).


% Get Path

pathDeletiveEditing(Open, Closed, Goal):-
	getBestChild(Open, [Goal, Parent, H], RestOfOpen),
    printsolution([Goal,Parent, H], Closed),!.
    
pathDeletiveEditing(Open, Closed, Goal):-
    getBestChild(Open, [State,Parent , H], RestOpen),
    getChildrenDeletive(State, Open, Closed, Children, H, Goal),
    addListToOpenBFS(Children , RestOpen, NewOpen),
	pathDeletiveEditing(NewOpen, [[State,Parent , H] | Closed], Goal).
   
deletiveEditing(Start, Goal):-
    getHeuristic(Start, H, Goal),
    pathDeletiveEditing([[Start,null, H]],[], Goal).

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