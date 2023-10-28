import { Inject, Service } from 'typedi';

import { FloorMap } from '../mappers/FloorMap';

import { Document, FilterQuery, Model } from 'mongoose';
import { UniqueEntityID } from '../core/domain/UniqueEntityID';
import { IFloorPersistence } from '../dataschema/IFloorPersistence';
import { Floor } from '../domain/Floor/floor';
import IFloorRepo from '../services/IRepos/IFloorRepo';

@Service()
export default class FloorRepo implements IFloorRepo {
  constructor(@Inject('floorSchema') private floorSchema: Model<IFloorPersistence & Document>) {}

  public async exists(floor: Floor): Promise<boolean> {
    const idX = floor.id instanceof UniqueEntityID ? (<UniqueEntityID>floor.id).toValue() : floor.id;

    const query = { domainId: idX };
    const floorDocument = await this.floorSchema.findOne(query as FilterQuery<IFloorPersistence & Document>);

    return Boolean(floorDocument) === true;
  }

  public async save(floor: Floor): Promise<Floor> {
    const query = { id: floor.id.toString() };

    const floorDocument = await this.floorSchema.findOne(query);

    try {
      if (floorDocument === null) {
        const rawFloor: any = FloorMap.toPersistence(floor);
        const floorCreated = await this.floorSchema.create(rawFloor);

        return FloorMap.toDomain(floorCreated) ? FloorMap.toDomain(floorCreated).getValue() : null;
      } else {
        floorDocument.floorNumber = floor.floorNumber;
        floorDocument.description = floor.description.value;
        floorDocument.servedByElevator = floor.servedByElevator;
        floorDocument.buildingCode = floor.buildingCode.value;

        await floorDocument.save();

        return floor;
      }
    } catch (error) {
      throw error;
    }
  }
}
