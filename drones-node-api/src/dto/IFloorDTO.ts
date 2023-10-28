export default interface IFloorDTO {
  id: string;
  floorNumber: number;
  description: string | null;
  servedByElevator: boolean;
  buildingCode: string;
}
