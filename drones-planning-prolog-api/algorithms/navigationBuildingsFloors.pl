% TRANSLATED EXAMPLE FROM MOODLE

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


% Rules are here for testing purposes
floors(a,[a1]).
floors(b,[b1,b2,b3,b4]).
floors(g,[g2,g3,g4]).
floors(h,[h1,h2,h3,h4]).
floors(i,[i1,i2,i3,i4]).
floors(j,[j1,j2,j3,j4]).
elevator(b,[b1,b2,b3,b4]).
elevator(g,[g2,g3,g4]).
elevator(i,[i1,i2,i3,i4]).
elevator(j,[j1,j2,j3,j4]).
corridor(a,h,a1,h2).
corridor(b,g,b2,g2).
corridor(b,g,b3,g3).
corridor(b,i,b3,i3).
corridor(g,h,g2,h2).
corridor(g,h,g3,h3).
corridor(h,i,h2,i2).
corridor(i,j,i1,j1).
corridor(i,j,i2,j2).
corridor(i,j,i3,j3).
connects(a,h).
connects(b,g).
connects(b,i).
connects(g,h).
connects(h,i).
connects(i,j).

path_buildings(BdOr,BdDest,LBdPath):-path_buildings2(BdOr,BdDest,[BdOr],LBdPath).

path_buildings2(BdX,BdX,LBdInv,LBdPath):-!,reverse(LBdInv,LBdPath).

path_buildings2(BdAct,BdDest,LBdPassed,LBdPath):-(connects(BdAct,BdInt);connects(BdInt,BdAct)), \+(member(BdInt,LBdPassed)), path_buildings2(BdInt,BdDest,[BdInt|LBdPassed],LBdPath).



all_path_buildings(BdOr,BdDest,LTPathBd):- findall(LBdPath,path_buildings(BdOr,BdDest,LBdPath),LTPathBd).



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
