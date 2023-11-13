:- module(logic, [client_get_buildings/1, list_building_codes/0]).

:- use_module(library(http/http_client)).

:- dynamic building_code/1.

% localhost:4000/api/building/all
client_get_buildings(Length) :-
  URL = 'http://localhost:4000/api/building/all',
  http_get(URL, Reply, [json_object(dict)]),
  is_list(Reply),
  length(Reply, Length),
  format('Length of the array is: ~w', [Length]),
  store_building_codes(Reply).

store_building_codes([]).
store_building_codes([H|T]) :-
  assert_building_code(H),
  store_building_codes(T).

assert_building_code(Building) :-
  BuildingCode = Building.get(code),
  assertz(building_code(BuildingCode)).

list_building_codes :-
  building_code(Code),
  format('Building Code: ~w~n', [Code]),
  fail.
list_building_codes.