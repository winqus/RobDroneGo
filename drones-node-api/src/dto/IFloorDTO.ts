export default interface IFloorDTO {
  id: string;
  floorNumber: number;
  description: string | null;
  servedByElevator: boolean;
  buildingCode: string;
  map?: {
    size: {
      width: number;
      height: number;
    };
    map: number[][];
    exitLocations?: {
      passages: { cellPosition: [number, number]; destination: { buildingCode: string; floorNumber: number } }[];
      elevators: { cellPosition: [number, number] }[];
    };
  };
}
