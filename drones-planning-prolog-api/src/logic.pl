:- module(logic, [client_get_buildings/1, list_building_codes/0, get_floors/0, get_elevators/0, get_passages/0]).

:- use_module(library(http/http_client)).
:- use_module(library(http/json)).

:- dynamic building_code/1.
:- dynamic json/1.

:- dynamic floors/2.
:- dynamic elevator/2.
:- dynamic corridor/4.
:- dynamic connects/2.

get_json_array_data(URL, JsonData) :-
    http_get(URL, JsonResponse, []),
    atom_json_term(JsonResponse, JsonData, []).

% localhost:4000/api/building/all
client_get_buildings(Length) :-
    URL = 'http://localhost:4000/api/building/all',
    get_json_array_data(URL, JsonData),
    is_list(JsonData),
    store_building_codes(JsonData),
    length(JsonData, Length).

store_building_codes([]).
store_building_codes([H|T]) :-
  assert_building_code(H),
  store_building_codes(T).

assert_building_code(Building) :-
  assertz(Building),
  json(Info),
  extract_code(Info, BuildingCode),
  retract(Building),
  assertz(building_code(BuildingCode)).

list_building_codes :-
  building_code(Code),
  format('Building Code: ~w~n', [Code]),
  fail.
list_building_codes.

extract_code(JSON, Code) :-
    member(code=Code, JSON).




get_floors() :-
 BaseUrl = 'http://localhost:4000/api/floor/',
 findall(Code, building_code(Code), ListCodes),
 add_code_to_end_url(BaseUrl, ListCodes, ListUrl),
 get_floors_by_buildings(ListUrl).

add_code_to_end_url(_, [], []).
add_code_to_end_url(BaseUrl, [Code | ListCodes], [[URL | Code] | ListUrl]) :-
  add_code_to_end_url(BaseUrl, ListCodes, ListUrl),
  string_concat(BaseUrl, Code, URL).

get_floors_by_buildings([]).
get_floors_by_buildings([[URL | Code] | ListUrl]) :-
  get_json_array_data(URL, JsonData),
  is_list(JsonData),
  extract_all_floorNumber(JsonData, FloorNumbers),
  assertz(floors(Code, FloorNumbers)),
  get_floors_by_buildings(ListUrl).


extract_all_floorNumber([], []).
extract_all_floorNumber([Json| ListJson] , [Number | FloorNumbers]) :-
  assertz(Json),
  json(Info),
  extract_floorNumber(Info, Number),
  retract(Json),
  extract_all_floorNumber(ListJson, FloorNumbers).

extract_floorNumber(JSON, FloorNumber) :-
    member(floorNumber=FloorNumber, JSON).


get_elevators() :-
  BaseUrl = 'http://localhost:4000/api/floor/elevator/?buildingCode=',
  findall(Code, building_code(Code), ListCodes),
  add_code_to_end_url(BaseUrl, ListCodes, ListUrl),
  get_elevators_by_buildings(ListUrl).

get_elevators_by_buildings([]).
get_elevators_by_buildings([[URL | Code] | ListUrl]) :-
  get_json_array_data(URL, JsonData),
  is_list(JsonData),
  extract_all_floorNumber(JsonData, FloorNumbers),
  assertz(elevator(Code, FloorNumbers)),
  get_elevators_by_buildings(ListUrl).



get_passages():-
  Url = 'http://localhost:4000/api/passage/',
  get_json_array_data(Url, JsonData),
  is_list(JsonData),
  assert_corridor_and_connects(JsonData).

assert_corridor_and_connects([]).
assert_corridor_and_connects([Json| ListJson]) :-
  assertz(Json),
  json(Info),
  extract_info(Info, Building1, Floor1, Building2, Floor2),
  retract(Json),
  assertz(corridor(Building1, Building2, Floor1, Floor2)),
  ((\+ ( connects(Building1, Building2) ; connects(Building2, Building1)), 
    assertz(connects(Building1, Building2))); true),
  assert_corridor_and_connects(ListJson).

extract_info(JSON, Building1, Floor1, Building2, Floor2) :-
    member(buildingCode1=Building1, JSON),
    member(buildingCode2=Building2, JSON),
    member(floorNumber1=Floor1, JSON),
    member(floorNumber2=Floor2, JSON).

