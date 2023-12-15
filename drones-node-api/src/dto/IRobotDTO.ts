export default interface IRobotDTO {
  id: string;
  code: string;
  description: string | null;
  nickname: string;
  serialNumber: string;
  available: boolean;
  type: string;
  position: {
    floorNumber: number;
    buildingCode: string;
    cellPosition: [number, number];
  };
}
