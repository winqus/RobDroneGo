import { Inject, Service } from 'typedi';
import config from '../../config';
import { Result } from '../core/logic/Result';
import { Building } from '../domain/Building/building';
import IRoomDTO from '../dto/IRoomDTO';
import { RoomMap } from '../mappers/RoomMap';
import IBuildingRepo from './IRepos/IBuildingRepo';
import IFloorRepo from './IRepos/IFloorRepo';
import IRoomRepo from './IRepos/IRoomRepo';
import IRoomService from './IServices/IRoomService';

@Service()
export default class RoomService implements IRoomService {
  constructor(
    @Inject(config.repos.room.name) private roomRepo: IRoomRepo,
    @Inject(config.repos.floor.name) private floorRepo: IFloorRepo,
    @Inject(config.repos.building.name) private buildingRepo: IBuildingRepo,
  ) {}

  public async createRoom(roomDTO: IRoomDTO): Promise<Result<IRoomDTO>> {
    try {
      const roomOrError = RoomMap.toDomain(roomDTO);
      if (roomOrError.isFailure) {
        return Result.fail<IRoomDTO>(roomOrError.errorValue().toString());
      }

      const room = roomOrError.getValue();

      const roomExists = await this.roomRepo.exists(room);
      if (roomExists) {
        return Result.fail<IRoomDTO>('Room already exists');
      }

      const floorExists = await this.floorRepo.findById(room.floorId.toString());
      if (!floorExists) {
        return Result.fail<IRoomDTO>('Floor does not exist');
      }

      const fitsInFloor = await this.validateFitInFloor(roomDTO, floorExists.buildingCode.value);
      if (fitsInFloor.isFailure) {
        return Result.fail<IRoomDTO>(fitsInFloor.errorValue().toString());
      }

      const noRoomOverlap = await this.validateNoRoomOverlap(roomDTO);
      if (noRoomOverlap.isFailure) {
        return Result.fail<IRoomDTO>(noRoomOverlap.errorValue().toString());
      }

      await this.roomRepo.save(room);

      const roomDTOResult = RoomMap.toDTO(room);

      return Result.ok<IRoomDTO>(roomDTOResult);
    } catch (err) {
      return Result.fail<IRoomDTO>(`Failed to create room: ${err}`);
    }
  }

  public async validateFitInFloor(roomDTO: IRoomDTO, buildingCode?: string): Promise<Result<boolean>> {
    let building: Building;
    if (!buildingCode) {
      const floor = await this.floorRepo.findById(roomDTO.floorId.toString());
      if (!floor) {
        return Result.fail<boolean>('Floor does not exist');
      }

      building = await this.buildingRepo.findByCode(floor.buildingCode.value);
      if (!building) {
        return Result.fail<boolean>('Building does not exist');
      }
    } else {
      building = await this.buildingRepo.findByCode(buildingCode);
      if (!building) {
        return Result.fail<boolean>('Building does not exist');
      }
    }
    const floorSize = building.floorSize.value;
    const roomSize = roomDTO.size;
    const roomPosition = roomDTO.position;

    if (roomPosition.x < 0 || roomPosition.y < 0) {
      return Result.fail<boolean>('Room position is invalid');
    }

    const roomFitsInFloor =
      roomSize.width + roomPosition.y <= floorSize.width && roomSize.length + roomPosition.x <= floorSize.length;

    if (!roomFitsInFloor) {
      return Result.fail<boolean>('Room does not fit in floor');
    }

    return Result.ok<boolean>(true);
  }

  public async validateNoRoomOverlap(roomDTO: IRoomDTO): Promise<Result<boolean>> {
    const floor = await this.floorRepo.findById(roomDTO.floorId);
    if (!floor) {
      return Result.fail<boolean>('Floor does not exist');
    }

    const rooms = await this.roomRepo.findAllInFloor(floor.id.toString());

    const newRoomEndX = roomDTO.position.x + roomDTO.size.length;
    const newRoomEndY = roomDTO.position.y + roomDTO.size.width;

    const roomOverlap = rooms.some((existingRoom) => {
      const existingRoomEndX = existingRoom.position.value.x + existingRoom.size.value.length;
      const existingRoomEndY = existingRoom.position.value.y + existingRoom.size.value.width;

      const xOverlap = roomDTO.position.x < existingRoomEndX && newRoomEndX > existingRoom.position.value.x;
      const yOverlap = roomDTO.position.y < existingRoomEndY && newRoomEndY > existingRoom.position.value.y;

      return xOverlap && yOverlap;
    });

    if (roomOverlap) {
      return Result.fail<boolean>('Room overlaps with another room');
    }

    return Result.ok<boolean>(true);
  }

  public async getRoomById(id: string): Promise<Result<IRoomDTO>> {
    try {
      const room = await this.roomRepo.findById(id);

      if (!room) {
        return Result.fail<IRoomDTO>('Room not found');
      }

      const roomDTO = RoomMap.toDTO(room);

      return Result.ok<IRoomDTO>(roomDTO);
    } catch (err) {
      return Result.fail<IRoomDTO>(`Failed to get room: ${err}`);
    }
  }

  public async getAllRooms(): Promise<Result<IRoomDTO[]>> {
    try {
      const rooms = await this.roomRepo.findAll();

      const roomDTOs = rooms.map((room) => RoomMap.toDTO(room));

      return Result.ok<IRoomDTO[]>(roomDTOs);
    } catch (err) {
      return Result.fail<IRoomDTO[]>(`Failed to get rooms: ${err}`);
    }
  }

  public async updateRoom(id: string, updates: Partial<IRoomDTO>): Promise<void> {
    try {
      const room = await this.roomRepo.findById(id);

      if (!room) {
        throw new Error('Room not found');
      }

      const roomOrError = RoomMap.toDomain({
        ...room,
        ...updates,
      });

      if (roomOrError.isFailure) {
        throw new Error(roomOrError.errorValue().toString());
      }

      await this.roomRepo.save(roomOrError.getValue());
    } catch (err) {
      throw new Error(`Failed to update room: ${err}`);
    }
  }

  public async deleteRoom(id: string): Promise<void> {
    try {
      await this.roomRepo.delete(id);
    } catch (err) {
      throw new Error(`Failed to delete room: ${err}`);
    }
  }
}
