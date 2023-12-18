export interface INavigationDataPersistence {
  floorsPaths: FloorPath[];
  mapPathCount: number;
  mapPaths: MapPath[];
}

interface FloorPath {
  fromBuilding: string;
  fromFloorNumber: string;
  toBuilding: string;
  toFloorNumber: string;
  type: string;
}

interface MapPath {
  buildingCode: string;
  cost: number;
  floorNumber: number;
  path: PathPoint[];
}

interface PathPoint {
  col: number;
  row: number;
}
