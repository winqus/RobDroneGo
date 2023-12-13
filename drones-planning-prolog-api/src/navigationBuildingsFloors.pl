:- module(navigationBuildingsFloors, [
  path_buildings/3,
  all_path_buildings/3,
  path_floors/4,
  better_path_floors/3,
  connection_list_cost/2
  ]).

:- include('../config.pl').
:- use_module(logic).

% floors(Building, FloorsList): Lists all floors available in each building.
% Usage example: floors(a, Floors).

% elevator(Building, FloorsList): Specifies the floors in a building that have elevator access.
% Usage example: elevator(b, FloorsWithElevator).

% corridor(Building1, Building2, Floor1, Floor2): Defines a corridor connecting a floor in one building to a floor in another.
% Usage example: corridor(a, h, a1, h2).

% connects(Building1, Building2): Indicates a general connection between two buildings.
% Usage example: connects(a, h).

% path_buildings(BuildingOrigin, BuildingDestination, BuildingsPath): Finds a path from one building to another.
% Usage example: path_buildings(a, j, Path).

% all_path_buildings(BuildingOrigin, BuildingDestination, AllPaths): Finds all possible building-level paths between two buildings.
% Usage example: all_path_buildings(a, j, AllPaths).

% path_floors(FloorOrigin, FloorDestination, BuildingsPath, ConnectionList): Finds a path from one floor to another, including the buildings path and the specific connections (elevators and corridors) used.
% Usage example: path_floors(a1, j4, BuildingsPath, ConnectionList).

% better_path_floors(FloorOrigin, FloorDestination, BestConnectionList): Determines the best path between two floors, minimizing elevator usage.
% Usage example: better_path_floors(a1, j4, BestPath).

% connection_list_cost(ConnectionList, Cost): Calculates the total cost of a given connection list.
% Usage example: connection_list_cost([elev(a1, a2), cor(a2, a3), elev(a3, a4)], Cost).

% Main method to find a single path between two buildings
path_buildings(BdOr,BdDest,LBdPath):-path_buildings2(BdOr,BdDest,[BdOr],LBdPath).

path_buildings2(BdX,BdX,LBdInv,LBdPath):-!,reverse(LBdInv,LBdPath).

path_buildings2(BdAct,BdDest,LBdPassed,LBdPath):-(connects(BdAct,BdInt);connects(BdInt,BdAct)), \+(member(BdInt,LBdPassed)), path_buildings2(BdInt,BdDest,[BdInt|LBdPassed],LBdPath).


% Main method to find all possible paths between two buildings
all_path_buildings(BdOr,BdDest,LTPathBd):- findall(LBdPath,path_buildings(BdOr,BdDest,LBdPath),LTPathBd).


% connection_list_cost(ConnectionList, Cost): Calculates the total cost of a given connection list.
% Corridor (cor) connections cost 2, and Elevator (elev) connections cost 4.
connection_list_cost(ConnectionList, Cost) :-
  connection_list_cost_helper(ConnectionList, 0, Cost).

connection_list_cost_helper([], Cost, Cost).

connection_list_cost_helper([elev(_, _)|Rest], CurrentCost, TotalCost) :-
  elevatorTransitCost(ElevatorCost),
  NewCost is CurrentCost + ElevatorCost,
  connection_list_cost_helper(Rest, NewCost, TotalCost).

connection_list_cost_helper([cor(_, _)|Rest], CurrentCost, TotalCost) :-
  passageTransitCost(PassageCost),
  NewCost is CurrentCost + PassageCost,
  connection_list_cost_helper(Rest, NewCost, TotalCost).


% Main method to find a single path between two floors
path_floors(FloorOr,FloorDest,LBdPath,LCon):- floors(BdOr,LFloorOr),member(FloorOr,LFloorOr),
floors(BdDest,LFloorsDest),member(FloorDest,LFloorsDest),path_buildings(BdOr,BdDest,LBdPath),
follow_floors(FloorOr,FloorDest,LBdPath,LCon).

follow_floors(FloorDest,FloorDest,_,[]).

follow_floors(FloorDest1,FloorDest,[BdDest],[elev(FloorDest1,FloorDest)]):- FloorDest\==FloorDest1,
elevator(BdDest,LFloors),member(FloorDest1,LFloors), member(FloorDest,LFloors).

follow_floors(FloorAct,FloorDest,[BdAct,BdNext|LOthersBd],[cor(FloorAct,FloorNext)|LOtherCon]):-
(corridor(BdAct,BdNext,FloorAct,FloorNext);corridor(BdNext,BdAct,FloorNext,FloorAct)),
follow_floors(FloorNext,FloorDest,[BdNext|LOthersBd],LOtherCon).

follow_floors(FloorAct,FloorDest,[BdAct,BdNext|LOthersBd],[elev(FloorAct,FloorAct1),cor(FloorAct1,FloorNext)|LOtherCon]):-
(corridor(BdAct,BdNext,FloorAct1,FloorNext); corridor(BdNext,BdAct,FloorNext,FloorAct1)),FloorAct1\==FloorAct,
elevator(BdAct,LFloors),member(FloorAct,LFloors),member(FloorAct1,LFloors),follow_floors(FloorNext,FloorDest,[BdNext|LOthersBd],LOtherCon).

better_path_floors(FloorOr,FloorDest,LBetterCon):- findall(LLCon,path_floors(FloorOr,FloorDest,_,LLCon),LLLCon),
less_elevators(LLLCon,LBetterCon,_,_).

less_elevators([LLCon],LLCon,NElev,NCor):- count(LLCon,NElev,NCor).

less_elevators([LCon|OthersLCon],LConR,NElevR,NCorR):- less_elevators(OthersLCon,LConM,NElev,NCor),count(LCon,NElev1,NCor1),
(((NElev1<NElev;(NElev1==NElev,NCor1<NCor)),!, NElevR is NElev1, NCorR is NCor1,LConR=LCon);
(NElevR is NElev,NCorR is NCor,LConR=LConM)).

count([],0,0).

count([elev(_,_)|L],NElev,NCor):- count(L,NElevL,NCor), NElev is NElevL+1.
count([cor(_,_)|L],NElev,NCor):-count(L,NElev,NCorL),NCor is NCorL+1.
