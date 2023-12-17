export interface NavigationPlan {
  floorsConnectionsCost: number;
  floorsPaths: FloorPath[];
  mapPathCount: number;
  mapPaths: MapPath[];
}

export interface FloorPath {
  fromBuilding: string;
  fromFloorNumber: string;
  toBuilding: string;
  toFloorNumber: string;
  type: string;
}

export interface MapPath {
  buildingCode: string;
  cost: number;
  floorNumber: number;
  path: PathPoint[];
}

export interface PathPoint {
  col: number;
  row: number;
}
