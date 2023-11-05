export default interface IRobotDTO {
  id: string;
  code: string;
  description: string | null;
  nickname: string;
  serialNumber: string;
  available: boolean;
  type: string;
}
