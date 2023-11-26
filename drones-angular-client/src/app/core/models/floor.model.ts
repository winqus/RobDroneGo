export default interface Floor {
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
    };
  }