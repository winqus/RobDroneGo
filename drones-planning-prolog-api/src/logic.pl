:- module(logic, [client_get_buildings/1, list_building_codes/0, get_floors/0, get_elevators/0, get_passages/0, load_info/0, remove_info/0, load_map/2, remove_map/0]).

:- use_module(library(http/http_client)).
:- use_module(library(http/json)).

:- dynamic building_code/1.
:- dynamic json/1.

:- dynamic floors/2. % floor (buildingCode, floorNumbers[])
:- dynamic elevator/2. % elevator (buildingCode, floorNumbers[])
:- dynamic corridor/4. % corridor (BuildingCode1, BuildingCode2, FloorNumber1, FloorNumber2)
:- dynamic connects/2. % corridor (BuildingCode1, BuildingCode2)

:- dynamic m/3. % m (column, row, value)
:- dynamic passage/4. % passage (column, row, buildingCode, floorNumber)
:- dynamic elevator_pos/2. % elevator_pos (column, row)

get_json_array_data(URL, JsonData) :-
    http_get(URL, JsonResponse, [status_code(Code)]),
    (   Code =:= 404 -> % If status code is 404
        JsonResponse = false  % Return false
    ;   true  % Otherwise, true
    ),
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
  BaseUrl = 'http://localhost:4000/api/floor/elevator?buildingCode=',
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
  
load_info():-
  client_get_buildings(_),
  get_floors(),
  get_elevators(),
  get_passages().

remove_info():-
  retractall(building_code(_)),
  retractall(floors(_,_)),
  retractall(elevator(_,_)),
  retractall(corridor(_,_,_,_)),
  retractall(connects(_,_)).


load_map(FloorNumber, BuildingCode):-
  generate_url_map(FloorNumber, BuildingCode, URL),
  get_json_array_data(URL, JsonData),
  assertz(JsonData),
  json(Info),
  extract_map(Info, Map),
  retract(JsonData),
  create_map(Map, 1),
  extract_exit_locations(Info, ExitLocations),
  extract_passages_elevators_exits(ExitLocations, Passages, Elevators),
  load_all_passage_exit(Passages),
  load_all_elevator_exit(Elevators).

remove_map():-
  retractall(m(_,_,_)),
  retractall(passage(_,_,_,_)),
  retractall(elevator_pos(_,_)).

generate_url_map(FloorNumber, BuildingCode, URL) :-
  BaseUrl = 'http://localhost:4000/api/floor/',
  MidleStr = '/building/',
  FinalStr = '/map',
  string_concat(BaseUrl, FloorNumber, URL_A),
  string_concat(URL_A, MidleStr, URL_B),
  string_concat(URL_B, BuildingCode, URL_C),
  string_concat(URL_C, FinalStr, URL).

extract_map(JsonData, Map) :-
  member(map=Map, JsonData).

create_map([], _).
create_map([Line|Map], Row) :-
  create_map_line(Line, Row, 1),
  Row1 is Row + 1,
  create_map(Map, Row1).

create_map_line([],_,_).
create_map_line([Value|Line], Row, Column):-
  assertz(m(Column, Row, Value)),
  Column1 is Column + 1,
  create_map_line(Line, Row, Column1).

extract_exit_locations(List, Info):-
  member(exitLocations=Json, List),
  assertz(Json),
  json(Info),
  retract(Json).

extract_passages_elevators_exits(List, Passages, Elevators):-
  member(passages=Passages, List),
  member(elevators=Elevators, List).


load_all_passage_exit([]).
load_all_passage_exit([Json|Passages]):-
  assertz(Json),
  json(Info),
  retract(Json),
  load_passage_exit(Info),
  load_all_passage_exit(Passages).

load_passage_exit(Info):-
  member(cellPosition=CellPosition, Info),
  member(destination=Destination, Info),
  assertz(Destination),
  json(DestinationValue),
  retract(Destination),
  member(buildingCode=BuildingCode, DestinationValue),
  member(floorNumber=FloorNumber, DestinationValue),
  [Row,Column] = CellPosition,
  assertz(passage(Column,Row,BuildingCode,FloorNumber)).

load_all_elevator_exit([]).
load_all_elevator_exit([Json|Elevators]):-
  assertz(Json),
  json(Info),
  retract(Json),
  load_elevator_exit(Info),
  load_all_passage_exit(Elevators).

load_elevator_exit(Info):-
  member(cellPosition=CellPosition, Info),
  [Row,Column] = CellPosition,
  assertz(elevator_pos(Column,Row)).




