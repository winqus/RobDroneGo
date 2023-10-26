export interface IFloorPersistence {
  id: string;
  code: string;
  description: string | null;
  servedByElevator: boolean;
  buildingCode: string;
}
