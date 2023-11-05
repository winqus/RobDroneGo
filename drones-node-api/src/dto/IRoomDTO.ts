export default interface IRoomDTO {
  id: string;
  name: string;
  description?: string;
  size: {
    width: number;
    length: number;
  };
  position: {
    x: number;
    y: number;
  };
  category: string;
  floorId: string;
}
