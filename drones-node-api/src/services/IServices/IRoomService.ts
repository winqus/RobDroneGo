import { Result } from '../../core/logic/Result';
import IRoomDTO from '../../dto/IRoomDTO';

export default interface IPassageService {
  createRoom(roomDTO: IRoomDTO): Promise<Result<IRoomDTO>>;
  validateFitInFloor(roomDTO: IRoomDTO, buildingCode?: string): Promise<Result<boolean>>;
  validateNoRoomOverlap(roomDTO: IRoomDTO): Promise<Result<boolean>>;
  getRoomById(id: string): Promise<Result<IRoomDTO | null>>;
  getAllRooms(): Promise<Result<IRoomDTO[]>>;
  updateRoom(id: string, updates: Partial<IRoomDTO>): Promise<void>;
  deleteRoom(id: string): Promise<void>;
}
