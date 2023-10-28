export interface IFloorPersistence {
  id: string;
  floorNumber: number;
  description: string | null;
  servedByElevator: boolean;
  buildingCode: string;
}
