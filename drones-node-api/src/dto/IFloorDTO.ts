export default interface IFloorDTO {
  id: string;
  code: string;
  description: string | null;
  servedByElevator: boolean;
  buildingCode: string;
}
