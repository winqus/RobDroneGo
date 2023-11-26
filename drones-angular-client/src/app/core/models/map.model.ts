export default interface Map {
  size: {
    width: number;
    height: number;
  };
  map: number[][];
  exitLocations?: {
    passages: { cellPosition: [number, number]; destination: { buildingCode: string; floorNumber: number } }[];
    elevators: { cellPosition: [number, number] }[];
  };
}
