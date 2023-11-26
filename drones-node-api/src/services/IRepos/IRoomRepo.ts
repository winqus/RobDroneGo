import { Repo } from '../../core/infra/Repo';
import { Room } from '../../domain/Room/room';
export default interface IRoomRepo extends Repo<Room> {
  save(room: Room): Promise<Room>;
  findById(id: string): Promise<Room | null>;
  findByName(name: string): Promise<Room | null>;
  findAll(): Promise<Room[]>;
  findAllInFloor(floorId: string): Promise<Room[]>;
  update(room: Room): Promise<void>;
  delete(id: string): Promise<void>;
}
