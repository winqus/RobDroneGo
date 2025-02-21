import { Document, FilterQuery, Model } from 'mongoose';
import { Inject, Service } from 'typedi';
import { UniqueEntityID } from '../core/domain/UniqueEntityID';
import { IRoomPersistence } from '../dataschema/IRoomPersistence';
import { Room } from '../domain/Room/room';
import { RoomMap } from '../mappers/RoomMap';
import IRoomRepo from '../services/IRepos/IRoomRepo';

@Service()
export default class RoomRepo implements IRoomRepo {
  constructor(@Inject('roomSchema') private roomSchema: Model<IRoomPersistence & Document>) {}

  public async exists(room: Room): Promise<boolean> {
    const idX = room.id instanceof UniqueEntityID ? room.id.toValue() : room.id;

    const queryById = { id: idX };
    const queryByName = { name: room.name.value };
    const roomDocumentById = await this.roomSchema.findOne(queryById as FilterQuery<IRoomPersistence & Document>);
    const roomDocumentByName = await this.roomSchema.findOne(queryByName as FilterQuery<IRoomPersistence & Document>);

    return Boolean(roomDocumentById) || Boolean(roomDocumentByName);
  }

  public async save(room: Room): Promise<Room> {
    const exists = await this.exists(room);

    const roomPersistence = RoomMap.toPersistence(room);

    if (exists) {
      const query = { id: room.id.toString() } as FilterQuery<IRoomPersistence & Document>;
      const roomDocument = await this.roomSchema.findOne(query);
      if (roomDocument) {
        await roomDocument.updateOne(roomPersistence);
      }
    } else {
      await this.roomSchema.create(roomPersistence);
    }

    return room;
  }

  public async findById(id: string): Promise<Room> {
    const query = { id: id } as FilterQuery<IRoomPersistence & Document>;
    const roomRecord = await this.roomSchema.findOne(query);
    const passageResult = RoomMap.toDomain(roomRecord);

    if (passageResult.isSuccess) {
      return passageResult.getValue();
    }

    return null;
  }

  public async findByName(name: string): Promise<Room> {
    const query = { name } as FilterQuery<IRoomPersistence & Document>;
    const roomRecord = await this.roomSchema.findOne(query);

    if (roomRecord) {
      const roomResult = RoomMap.toDomain(roomRecord);

      return roomResult.isSuccess ? roomResult.getValue() : null;
    }

    return null;
  }

  public async findAllInFloor(floorId: string): Promise<Room[]> {
    const query = { floorId } as FilterQuery<IRoomPersistence & Document>;
    const roomRecords = await this.roomSchema.find(query);

    return roomRecords
      .map((roomRecord) => {
        const roomResult = RoomMap.toDomain(roomRecord);

        return roomResult.isSuccess ? roomResult.getValue() : null;
      })
      .filter((room): room is Room => room !== null);
  }

  public async findAll(): Promise<Room[]> {
    const roomRecords = await this.roomSchema.find();

    return roomRecords.map((roomRecord) => {
      const roomResult = RoomMap.toDomain(roomRecord);

      return roomResult.isSuccess ? roomResult.getValue() : null;
    });
  }

  public async update(room: Room): Promise<void> {
    const roomPersistence = RoomMap.toPersistence(room);
    const query = { id: room.id.toString() } as FilterQuery<IRoomPersistence & Document>;
    const roomDocument = await this.roomSchema.findOne(query);
    if (roomDocument) {
      await roomDocument.updateOne(roomPersistence);
    }
  }

  public async delete(id: string): Promise<void> {
    const query = { id: id } as FilterQuery<IRoomPersistence & Document>;
    const roomDocument = await this.roomSchema.findOne(query);
    if (roomDocument) {
      await roomDocument.deleteOne();
    }
  }
}
