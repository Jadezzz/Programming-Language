input_edges(N) :- 
    N > 0 
    -> readln([A|R]), nth0(0, R, B), assert(edge(A, B)), assert(edge(B, A)), input_edges(N-1);
    write("Start Query..."), nl, readln([M|_]), query(M).

query(M) :-
    M > 0
    -> readln([A|R]), nth0(0, R, B), 
        (start_dest(A, B) -> write("Yes"), nl; write("No"), nl), query(M-1);halt.

start_dest(S,D) :-
    start_dest_(S,D,[]).

start_dest_(D,D,_Visited).        
start_dest_(S,D,Visited) :-       
   maplist(dif(S),Visited),       
   edge(S,X),            
   start_dest_(X,D,[S|Visited]).  
                                  
main :- 
    write("Input Edges..."), nl, 
    readln([_|R]), nth0(0, R, N), input_edges(N).

:- initialization(main).
