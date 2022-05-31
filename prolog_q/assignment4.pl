/* YOUR CODE HERE (Problem 1, delete the following line) */
range(S,E,M) :- M>=S, M=<E.
?- range(1,2,2).
?- not(range(1,2,3)).

/* YOUR CODE HERE (Problem 2, delete the following line) */
reverseL([],[]).
reverseL([X],[X]).
reverseL([ H | T], RevX ) :- reverseL(T, Rev), append(Rev,[H],RevX).

?- reverseL([],X).
?- reverseL([1,2,3],X).
?- reverseL([a,b,c],X).

/* YOUR CODE HERE (Problem 3, delete the following line) */
memberL(_,[]) :- false.
memberL(A,[H | T]) :- A=H ; memberL(A,T).



?- not(memberL(1, [])).
?- memberL(1,[1,2,3]).
?- not(memberL(4,[1,2,3])).
?- memberL(X, [1,2,3]).

/* YOUR CODE HERE (Problem 4, delete the following line) */
zip([], [_|_], []).
zip([_|_], [], []).
zip([], [], []).
zip([H | T], [A | B], [ H-A | X]) :- zip(T, B, X). 

?- zip([1,2],[a,b],Z).
?- zip([a,b,c,d], [1,X,y], Z).
?- zip([a,b,c],[1,X,y,z], Z).
?- length(A,2), length(B,2), zip(A, B, [1-a, 2-b]).

/* YOUR CODE HERE (Problem 5, delete the following line) */
insert([], [], []).
insert([], Ys, Ys).
insert(X, [], [X]).
insert(X, [H|T], [H|Zs]) :- X>H, insert(X, T, Zs).
insert(X, [H|T], [X,H|Zs]) :- X=<H, insert([], T, Zs).

?- insert(3, [2,4,5], L).
?- insert(3, [1,2,3], L).
?- not(insert(3, [1,2,4], [1,2,3])).
?- insert(3, L, [2,3,4,5]).
?- insert(9, L, [1,3,6,9]).
?- insert(3, L, [1,3,3,5]).

/* YOUR CODE HERE (Problem 6, delete the following line) */
removeN(A,[],[]).
removeN(A,[A|C],Y) :- removeN(A,C,Y).
removeN(A, [B|C], [B|Y]) :- removeN(A,C,Y).
remove_duplicates([],[]).
remove_duplicates([H|T],L2) :- removeN(H,T,Y), remove_duplicates(Y, L3), append([H], L3, L2).

?- remove_duplicates([1,2,3,4,2,3],X).
?- remove_duplicates([1,4,5,4,2,7,5,1,3],X).
?- remove_duplicates([], X).

/* YOUR CODE HERE (Problem 7, delete the following line) */
intersectionL(L1,[],[]).
intersectionL([],L2,[]).
intersectionL([],[],[]).
intersectionL([H|T], L2, L3) :- memberL(H,L2),intersectionL(T, L2, L4), append([H],L4,L3) ; intersectionL(T,L2,L3).

?- intersectionL([1,2,3,4],[1,3,5,6],[1,3]).
?- intersectionL([1,2,3,4],[1,3,5,6],X).
?- intersectionL([1,2,3],[4,3],[3]).

prefix(P,L) :- append(P,_,L).
suffix(S,L) :- append(_,S,L).

/* YOUR CODE HERE (Problem 8, delete the following line) */
partition([],[],[]). 
partition([H],[H],[]).
partition(L,P,S) :- length(L,N), P1 is div(N,2), S1 is (N-div(N,2)),length(P,P1),length(S,S1),prefix(P, L), suffix(S,L).
 

?- partition([a],[a],[]).
?- partition([1,2,3],[1],[2,3]).
?- partition([a,b,c,d],X,Y).

/* YOUR CODE HERE (Problem 9, delete the following line) */
merge([],[],[]).
merge(X,[],X).
merge([],Y,Y).
merge([H|T],[H1|T1],[H|T2]) :- H<H1, merge(T, [H1|T1], T2).
merge([H|T],[H1|T1],[H1|T2]) :- H1<H, merge([H|T], T1, T2).

?- merge([],[1],[1]).
?- merge([1],[],[1]).
?- merge([1,3,5],[2,4,6],X).

/* YOUR CODE HERE (Problem 10, delete the following line) */
mergesort([],[]).
mergesort([A],[A]).
mergesort([A,B],C) :- A=<B, C = [A,B] ; C = [B,A].
mergesort(L,LS) :- partition(L,A,B), mergesort(A,C), mergesort(B,D), merge(C,D,LS).

?- mergesort([3,2,1],X).
?- mergesort([1,2,3],Y).
?- mergesort([],Z).
