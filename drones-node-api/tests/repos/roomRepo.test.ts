import { MockProxy, mock } from 'jest-mock-extended';
import { Document, Model } from 'mongoose';
import { UniqueEntityID } from '../../src/core/domain/UniqueEntityID';
import { IRoomPersistence } from '../../src/dataschema/IRoomPersistence';
import { RoomCategory } from '../../src/domain/Room/ValueObjects/category';
import { Room } from '../../src/domain/Room/room';
import { RoomMap } from '../../src/mappers/RoomMap';
import RoomRepo from '../../src/repos/roomRepo';

let roomRepo: RoomRepo;
let roomStub: Room;
let roomPersistenceStub: IRoomPersistence;
let roomDocumentMock: MockProxy<Model<Room & Document>>;
let roomSchemaMock: MockProxy<Model<any & Document>> & Model<any & Document>;

beforeEach(() => {
  roomSchemaMock = mock<Model<any & Document>>();
  roomRepo = new RoomRepo(roomSchemaMock);
  roomStub = {
    id: new UniqueEntityID(),
    name: { value: 'Room 1' },
    description: { value: 'Room 1 description' },
    category: RoomCategory.Laboratory,
    size: { value: { length: 10, width: 10 } },
    position: { value: { x: 10, y: 10 } },
    floorId: new UniqueEntityID(),
  } as Room;
  roomPersistenceStub = RoomMap.toPersistence(roomStub);
  roomDocumentMock = mock<Model<Room & Document>>();
});

describe('RoomRepo', () => {
  it('should save a new room', async () => {
    roomSchemaMock.create.mockResolvedValueOnce(roomStub as any);

    await roomRepo.save(roomStub);

    expect(roomSchemaMock.create).toBeCalledWith(RoomMap.toPersistence(roomStub));
  });

  it('should update a room', async () => {
    const foundRoomMock = {
      updateOne: jest.fn().mockResolvedValueOnce({ nModified: 1 }),
    };
    roomSchemaMock.findOne.mockResolvedValueOnce(foundRoomMock as any);

    await roomRepo.update(roomStub);

    expect(roomSchemaMock.findOne).toBeCalledWith({ id: roomStub.id.toString() });
    expect(foundRoomMock.updateOne).toBeCalledWith(RoomMap.toPersistence(roomStub));
  });

  it('should find a room by id', async () => {
    roomSchemaMock.findOne.mockResolvedValueOnce(roomPersistenceStub as any);

    const foundRoom = await roomRepo.findById(roomStub.id.toString());

    expect(foundRoom).not.toBeNull();
    expect(RoomMap.toPersistence(foundRoom)).toEqual(roomPersistenceStub);
    expect(roomSchemaMock.findOne).toBeCalledWith({ id: roomStub.id.toString() });
  });

  it('should find a room by name', async () => {
    roomSchemaMock.findOne.mockResolvedValueOnce(roomPersistenceStub as any);

    const foundRoom = await roomRepo.findByName(roomStub.name.value);

    expect(foundRoom).not.toBeNull();
    expect(RoomMap.toPersistence(foundRoom)).toEqual(roomPersistenceStub);
    expect(roomSchemaMock.findOne).toBeCalledWith({ name: roomStub.name.value });
  });

  it('should find all rooms in a floor', async () => {
    roomSchemaMock.find.mockResolvedValueOnce([roomPersistenceStub, roomPersistenceStub] as any);

    const rooms = await roomRepo.findAllInFloor(roomStub.floorId.toString());

    expect(rooms.length).toEqual(2);
    expect(roomSchemaMock.find).toBeCalledWith({ floorId: roomStub.floorId.toString() });
  });

  it('should find all rooms', async () => {
    roomSchemaMock.find.mockResolvedValueOnce([roomPersistenceStub] as any);

    const rooms = await roomRepo.findAll();

    expect(rooms.length).toEqual(1);
    expect(roomSchemaMock.find).toBeCalled();
  });

  it('should delete a room by id', async () => {
    roomDocumentMock.deleteOne.mockResolvedValueOnce({ deletedCount: 1 } as any);
    roomSchemaMock.findOne.mockResolvedValueOnce(roomDocumentMock as any);

    await roomRepo.delete(roomStub.id.toString());

    expect(roomDocumentMock.deleteOne).toBeCalled();
  });
});
