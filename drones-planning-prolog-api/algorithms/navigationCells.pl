//m/3 rules are used for testing purposes //

m(1,1,1).
m(2,1,1).
m(3,1,1).
m(4,1,1).
m(5,1,1).
m(6,1,1).
m(7,1,1).
m(8,1,1).
m(1,2,0).
m(2,2,0).
m(3,2,0).
m(4,2,0).
m(5,2,0).
m(6,2,0).
m(7,2,0).
m(8,2,1).
m(1,3,0).
m(2,3,0).
m(3,3,0).
m(4,3,0).
m(5,3,0).
m(6,3,0).
m(7,3,0).
m(8,3,1).
m(1,4,0).
m(2,4,0).
m(3,4,0).
m(4,4,0).
m(5,4,0).
m(6,4,0).
m(7,4,0).
m(8,4,1).
m(1,5,1).
m(2,5,1).
m(3,5,1).
m(4,5,1).
m(5,5,0).
m(6,5,0).
m(7,5,0).
m(8,5,1).
m(1,6,1).
m(2,6,1).
m(3,6,1).
m(4,6,1).
m(5,6,0).
m(6,6,0).
m(7,6,0).
m(8,6,1).
m(1,7,1).
m(2,7,1).
m(3,7,1).
m(4,7,1).
m(5,7,0).
m(6,7,0).
m(7,7,0).
m(8,7,1).

:-dynamic ligacel/2.

create_graph(_,0):-!.

create_graph(Col,Lin):-create_graph_lin(Col,Lin),Lin1 is Lin-1,create_graph(Col,Lin1).

create_graph_lin(0,_):-!.

create_graph_lin(Col,Lin):- m(Col,Lin,0),!,ColS is Col+1, ColA is Col-1, LinS is Lin+1,LinA is Lin-1,((m(ColS,Lin,0),
assertz(ligacel(cel(Col,Lin),cel(ColS,Lin)));true)),
((m(ColA,Lin,0),assertz(ligacel(cel(Col,Lin),cel(ColA,Lin)));true)),
((m(Col,LinS,0),assertz(ligacel(cel(Col,Lin),cel(Col,LinS)));true)),
((m(Col,LinA,0),assertz(ligacel(cel(Col,Lin),cel(Col,LinA)));true)),
Col1 is Col-1, create_graph_lin(Col1,Lin).

create_graph_lin(Col,Lin):-Col1 is Col-1,create_graph_lin(Col1,Lin).


dfs(Orig,Dest,Path):- dfs2(Orig,Dest,[Orig],Path).

dfs2(Dest,Dest,LA,Path):- reverse(LA,Path).

dfs2(Act,Dest,LA,Path):-ligacel(Act,X),\+ member(X,LA), dfs2(X,Dest,[X|LA],Path).



all_dfs(Orig,Dest,LPath):-findall(Path,dfs(Orig,Dest,Path),LPath).



better_dfs(Orig,Dest,Path):- all_dfs(Orig,Dest,LPath), shortlist(LPath,Path,_).

shortlist([L],L,N):-!,length(L,N).

shortlist([L|LL],Lm,Nm):-shortlist(LL,Lm1,Nm1), length(L,NL), ((NL<Nm1,!,Lm=L,Nm is NL);(Lm=Lm1,Nm is Nm1)).




bfs(Orig,Dest,Path):-bfs2(Dest,[[Orig]],Path).

bfs2(Dest,[[Dest|T]|_],Path):- reverse([Dest|T],Path).

bfs2(Dest,[LA|Outros],Path):- LA=[Act|_], findall([X|LA], (Dest\==Act,ligacel(Act,X),\+ member(X,LA)),Novos), append(Outros,Novos,Todos), bfs2(Dest,Todos,Path).


all_bfs(Orig,Dest,LPath):-findall(Path,bfs(Orig,Dest,Path),LPath).


shortest_bfs(Orig, Dest, ShortestPath) :- all_bfs(Orig, Dest, AllPaths), shortlist(AllPaths, ShortestPath, _).
