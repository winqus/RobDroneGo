// Changes need to be applied to src/app/ThreeDModule/mapCell.js as well
export enum MapCell {
  NoNorthWallNoWestWall = 0,
  NoNorthWallYesWestWall = 1,
  YesNorthWallNoWestWall = 2,
  YesNorthWallYesWestWall = 3,
  DoorNorth = 4,
  DoorWest = 5,
  PassageNorth = 6,
  PassageWest = 7,
  ElevatorNorth = 8,
  ElevatorSouth = 9,
  ElevatorWest = 10,
  ElevatorEast = 11,
}
