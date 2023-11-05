export interface IFloorPersistence {
  id: string;
  floorNumber: number;
  description: string | null;
  servedByElevator: boolean;
  buildingCode: string;
  map?: null | {
    width: number;
    height: number;
    map: number[][];
  };
}
