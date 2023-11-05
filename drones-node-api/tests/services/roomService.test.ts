import { MockProxy, mock } from 'jest-mock-extended';
import { UniqueEntityID } from '../../src/core/domain/UniqueEntityID';
import { Result } from '../../src/core/logic/Result';
import { RoomCategory } from '../../src/domain/Room/ValueObjects/category';
import { RoomMap } from '../../src/mappers/RoomMap';
import IBuildingRepo from '../../src/services/IRepos/IBuildingRepo';
import IFloorRepo from '../../src/services/IRepos/IFloorRepo';
import IRoomRepo from '../../src/services/IRepos/IRoomRepo';
import RoomService from '../../src/services/roomService';

let roomRepoMock: MockProxy<IRoomRepo>;
let floorRepoMock: MockProxy<IFloorRepo>;
let buildingRepoMock: MockProxy<IBuildingRepo>;
let roomService: RoomService;
const roomDTOstub = {
  name: 'Room 1',
  description: 'Room 1 description',
  category: RoomCategory.Amphitheater.valueOf(),
  size: {
    width: 10,
    length: 10,
  },
  position: {
    x: 3,
    y: 3,
  },
  floorId: new UniqueEntityID().toString(),
};
const roomStub = RoomMap.toDomain(roomDTOstub).getValue();

beforeEach(() => {
  roomRepoMock = mock<IRoomRepo>();
  floorRepoMock = mock<IFloorRepo>();
  buildingRepoMock = mock<IBuildingRepo>();
  roomService = new RoomService(roomRepoMock, floorRepoMock, buildingRepoMock);
});

describe('RoomRepo', () => {
  it('should create a new room successfully', async () => {
    const roomOrError = RoomMap.toDomain(roomDTOstub);
    RoomMap.toDomain = jest.fn().mockReturnValue(roomOrError);
    roomRepoMock.exists.mockResolvedValue(false as any);
    roomRepoMock.findAllInFloor.mockResolvedValue([] as any);
    floorRepoMock.findById.mockResolvedValue({ id: 'some-id', buildingCode: { value: 'building-code' } } as any);
    buildingRepoMock.findByCode.mockResolvedValue({ floorSize: { value: { length: 100, width: 100 } } } as any);

    const result = await roomService.createRoom(roomDTOstub as any);

    expect(RoomMap.toDomain).toBeCalledWith(roomDTOstub);
    expect(result.isSuccess).toBe(true);
    expect(roomRepoMock.save).toHaveBeenCalled();
  });

  it('should fail to create a room if it already exists', async () => {
    roomRepoMock.exists.mockResolvedValue(true as any);

    const result = await roomService.createRoom(roomDTOstub as any);

    expect(result.isFailure).toBe(true);
    expect(result.errorValue()).toEqual('Room already exists');
  });

  it('should fail to create a room if it does not fit in the floor', async () => {
    roomRepoMock.exists.mockResolvedValue(false as any);
    floorRepoMock.findById.mockResolvedValue({ buildingCode: { value: 'building-code' } } as any);
    buildingRepoMock.findByCode.mockResolvedValue({ floorSize: { value: { length: 100, width: 100 } } } as any);

    const result = await roomService.createRoom({ ...roomDTOstub, size: { width: 100, length: 100 } } as any);

    expect(result.isFailure).toBe(true);
    expect(result.errorValue()).toEqual('Room does not fit in floor');
  });

  it('should create two rooms in the same floor', async () => {
    roomRepoMock.exists.mockResolvedValue(false as any);
    roomRepoMock.findAllInFloor.mockResolvedValueOnce([] as any);
    roomRepoMock.findAllInFloor.mockResolvedValueOnce([roomStub] as any);
    floorRepoMock.findById.mockResolvedValue({ id: 'some-id', buildingCode: { value: 'building-code' } } as any);
    buildingRepoMock.findByCode.mockResolvedValue({ floorSize: { value: { length: 100, width: 100 } } } as any);

    const result1 = await roomService.createRoom(roomDTOstub as any);
    const result2 = await roomService.createRoom({
      ...roomDTOstub,
      position: { x: 0, y: 0 },
      size: { width: 3, length: 3 },
    } as any);

    expect(result1.isSuccess).toBe(true);
    expect(result2.isSuccess).toBe(true);
    expect(roomRepoMock.save).toHaveBeenCalledTimes(2);
  });

  it('should fail to create a room if it overlaps with another room', async () => {
    roomRepoMock.exists.mockResolvedValue(false as any);
    const roomStub2 = { ...roomStub.props, position: { value: { x: 2, y: 2 } }, id: 'some-id' };
    roomRepoMock.findAllInFloor.mockResolvedValue([roomStub2] as any);
    floorRepoMock.findById.mockResolvedValue({ id: 'some-id', buildingCode: { value: 'building-code' } } as any);
    buildingRepoMock.findByCode.mockResolvedValue({ floorSize: { value: { length: 100, width: 100 } } } as any);

    const result = await roomService.createRoom(roomDTOstub as any);

    expect(result.isFailure).toBe(true);
    expect(result.errorValue()).toEqual('Room overlaps with another room');
  });

  it('should return a room by ID', async () => {
    roomRepoMock.findById.mockResolvedValue(roomStub as any);
    RoomMap.toDTO = jest.fn().mockReturnValue(roomDTOstub);

    const result = await roomService.getRoomById('1');

    expect(result.isSuccess).toBe(true);
    expect(RoomMap.toDTO).toBeCalledWith(roomStub);
    expect(result.getValue()).toEqual(roomDTOstub);
  });

  it('should return not found when getting a non-existing room by ID', async () => {
    roomRepoMock.findById.mockResolvedValue(null as any);

    const result = await roomService.getRoomById('1');

    expect(result.isFailure).toBe(true);
    expect(result.errorValue()).toEqual('Room not found');
  });

  it('should update a room successfully', async () => {
    const updatedRoomDTO = { ...roomDTOstub, name: 'Updated Name' };
    roomRepoMock.findById.mockResolvedValue(roomStub as any);
    RoomMap.toDomain = jest.fn().mockReturnValue(Result.ok(roomStub));

    await roomService.updateRoom('1', updatedRoomDTO);

    expect(RoomMap.toDomain).toHaveBeenCalled();
    expect(roomRepoMock.save).toHaveBeenCalledWith(roomStub);
  });

  it('should fail to update a non-existing room', async () => {
    roomRepoMock.findById.mockResolvedValue(null as any);

    await expect(roomService.updateRoom('1', roomDTOstub)).rejects.toThrow('Room not found');
  });

  it('should delete a room successfully', async () => {
    await roomService.deleteRoom('1');

    expect(roomRepoMock.delete).toHaveBeenCalledWith('1');
  });

  it('should throw an error if deletion fails', async () => {
    roomRepoMock.delete.mockRejectedValue(new Error('Delete failed'));

    await expect(roomService.deleteRoom('1')).rejects.toThrow('Failed to delete room: Error: Delete failed');
  });
});
