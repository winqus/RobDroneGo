:- module(route_handler, [get_route_handler/1]).

:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).

:- use_module(logic).


:- http_handler('/planning-api/route', get_route_handler, []).

:- http_handler('/planning-api/test', get_route_test_handler, []).
:- http_handler('/planning-api/', get_test_handler, []).
:- http_handler('/', get_test_handler, []).

%  Route route with parameters handler
get_route_handler(Request) :-
  http_parameters(Request, 
    [ origin_building_code(OriginBuildingCode, []),
      origin_floor_number(OriginFloorNumber, []),
      origin_map_cell_x(OriginMapCellX, []),
      origin_map_cell_y(OriginMapCellY, []),
      destination_building_code(DestinationBuildingCode, []),
      destination_floor_number(DestinationFloorNumber, []),
      destination_map_cell_x(DestinationMapCellX, []),
      destination_map_cell_y(DestinationMapCellY, []),
      minimize_elevator_uses(MinimizeElevatorUses, []),
      minimize_building_count(MinimizeBuildingCount, [])
    ]),
  logic:load_info(), % GETs building, floor, elevator, passage info from backend API
  format('Content-type: application/json~n~n'),
  format('{"origin_building_code": "~w",~n', [OriginBuildingCode]),
  format('"origin_floor_number": "~w",~n', [OriginFloorNumber]),
  format('"origin_map_cell_x": "~w",~n', [OriginMapCellX]),
  format('"origin_map_cell_y": "~w",~n', [OriginMapCellY]),
  format('"destination_building_code": "~w",~n', [DestinationBuildingCode]),
  format('"destination_floor_number": "~w",~n', [DestinationFloorNumber]),
  format('"destination_map_cell_x": "~w",~n', [DestinationMapCellX]),
  format('"destination_map_cell_y": "~w",~n', [DestinationMapCellY]),
  format('"minimize_elevator_uses": "~w",~n', [MinimizeElevatorUses]),
  format('"minimize_building_count": "~w"~n', [MinimizeBuildingCount]).
  % logic:remove_info(). % Removes building, floor, elevator, passage, connects info from backend API

% Test route handler that returns a simple JSON response
get_test_handler(_Request) :-
  format('Content-type: application/json~n~n'),
  format('{"message": "This is a simple JSON response from Planning API"}').

% Test route handler that returns request query parameters
get_route_test_handler(Request) :-
  http_parameters(Request, 
      [ origin_building_code(OriginBuildingCode, []),
        origin_floor_number(OriginFloorNumber, []),
        origin_map_cell_x(OriginMapCellX, []),
        origin_map_cell_y(OriginMapCellY, []),
        destination_building_code(DestinationBuildingCode, []),
        destination_floor_number(DestinationFloorNumber, []),
        destination_map_cell_x(DestinationMapCellX, []),
        destination_map_cell_y(DestinationMapCellY, []),
        minimize_elevator_uses(MinimizeElevatorUses, []),
        minimize_building_count(MinimizeBuildingCount, [])
      ]),
  format('Content-type: text/plain~n~n'),
  format('Origin Building Code: ~w~n', [OriginBuildingCode]),
  format('Origin Floor Number: ~w~n', [OriginFloorNumber]),
  format('Origin Map Cell X: ~w~n', [OriginMapCellX]),
  format('Origin Map Cell Y: ~w~n', [OriginMapCellY]),
  format('Destination Building Code: ~w~n', [DestinationBuildingCode]),
  format('Destination Floor Number: ~w~n', [DestinationFloorNumber]),
  format('Destination Map Cell X: ~w~n', [DestinationMapCellX]),
  format('Destination Map Cell Y: ~w~n', [DestinationMapCellY]),
  format('Minimize Elevator Uses: ~w~n', [MinimizeElevatorUses]),
  format('Minimize Building Count: ~w~n', [MinimizeBuildingCount]).