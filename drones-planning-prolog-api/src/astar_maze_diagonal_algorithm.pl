:- module(astar_maze_diagonal_algorithm, [aStar/4, bestFirst/4, beamSearch/4, beam_search/5]).

% Uses graph_creation_diagonal and replaced estimate to support diagonal movement, other than that it's the same as astar_algorithm
:- use_module(graph_creation_for_maze_diagonal).

% graph_creation_for_maze_diagonal:create_graph(4,4).
% aStar(cel(1, 1), cel(4, 4), Path, Cost), writeln(Path). % quick
% aStar(cel(3, 2), cel(2, 3), Path, Cost), writeln(Path).

:- dynamic cached_estimate/3.

clear_estimate_cache :-
	retractall(cached_estimate(_, _, _)).

% Estimate function (Euclidean distance, supports diagonal) with caching
estimate(cel(Col1, Row1), cel(Col2, Row2), Estimate) :-
    % Check if the estimate is already cached
    (  cached_estimate(cel(Col1, Row1), cel(Col2, Row2), CachedEstimate)
    -> Estimate = CachedEstimate  % Use cached value
    ;  % Else compute and cache the estimate
       DCol is Col1 - Col2,
       DRow is Row1 - Row2,
       Estimate is sqrt(DCol * DCol + DRow * DRow),
       assertz(cached_estimate(cel(Col1, Row1), cel(Col2, Row2), Estimate))
    ).

%% Similar to aStar but keeping only the most promisable partial path
bestFirst(Start, End, Path, Cost) :-
	clear_estimate_cache,
	bestFirst2(End, (_, 0, [Start]), Path, Cost).

bestFirst2(End, (_, Cost, [End | T]), Path, Cost) :-
		reverse([End | T], Path).

bestFirst2(End, (_, Ca, LA), Path, Cost) :-
		LA = [Act | _],
		findall((CEX, CaX, [X | LA]),
						(End \== Act, connectCell(Act, X, CostX), \+ member(X, LA),
						CaX is CostX + Ca, estimate(X, End, EstX),
						CEX is CaX + EstX), New),
		sort(New, [B | _]),
		write('Path bf2 is: '), writeln(B),
		bestFirst2(End, B, Path, Cost).

%% Beam Search Implementation. Beam width is 40 by default.
beamSearch(Start, End, Path, Cost) :-
	beam_search(Start, End, Path, Cost, 40).

beam_search(Start, Goal, Path, Cost, BeamWidth) :-
	clear_estimate_cache,
	beam_search_helper(Goal, BeamWidth, [(_, 0, [Start])], Path, Cost).

beam_search_helper(Goal, _, [(_, Cost, [Goal | T]) | _], Path, Cost) :-
	reverse([Goal | T], Path).

beam_search_helper(Goal, BeamWidth, [(_, Ca, LA) | Others], Path, Cost) :-
	LA = [Act | _],
	findall((CEX, CaX, [X | LA]),
					(Goal \== Act, connectCell(Act, X, CostX), \+ member(X, LA),
					CaX is CostX + Ca, estimate(X, Goal, EstX),
					CEX is CaX + EstX), New),
	append(Others, New, All),
	sort(All, AllOrd),
	trim_beam(AllOrd, BeamWidth, Trimmed),
	beam_search_helper(Goal, BeamWidth, Trimmed, Path, Cost).

% Trims the list to only keep the top N elements as per the beam width
trim_beam(List, BeamWidth, Trimmed) :-
	length(List, Length),
	(Length > BeamWidth -> length(Trimmed, BeamWidth); Trimmed = List),
	append(Trimmed, _, List).
	



% A* Algorithm Implementation
aStar(Start, End, Path, Cost) :-
	clear_estimate_cache,
	aStar2(End, [(_, 0, [Start])], Path, Cost).

aStar2(End, [(_, Cost, [End | T]) | _], Path, Cost) :-
	reverse([End | T], Path).

aStar2(End, [(_, Ca, LA) | Others], Path, Cost) :-
	LA = [Act | _],
	findall((CEX, CaX, [X | LA]),
					(End \== Act, connectCell(Act, X, CostX), \+ member(X, LA),
					CaX is CostX + Ca, estimate(X, End, EstX),
					CEX is CaX + EstX), New),
	append(Others, New, All),
	sort(All, AllOrd),
	aStar2(End, AllOrd, Path, Cost).
