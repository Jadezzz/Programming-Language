ancestor(A,B) :- parent(A,B). 
ancestor(A,B) :- parent(X,B),ancestor(A,X). 

lca(A,B) :- 
  A==B -> write("LCA is: "), write(A), nl;
  ancestor(A,B) -> write("LCA is: "), write(A), nl;
  parent(X,A),lca(X,B).
  
add_relation(N) :- 
  N > 0  
  -> readln([A|R]), nth0(0, R, B), assert(parent(A, B)), add_relation(N-1);
  write("Start Query..."), nl, readln([M|_]), query_lca(M).
  
query_lca(M) :- 
  M > 0
  -> readln([A|R]), nth0(0, R, B), lca(A, B), query_lca(M-1);
  halt.
   
main :- 
  write("Input Relations..."), nl, 
  readln([N|_]), add_relation(N-1).

:- initialization(main).
