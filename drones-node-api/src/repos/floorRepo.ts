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

  public async findByCode(buildingCode: string, floorNumber: number): Promise<Floor> {
    const query = { buildingCode: buildingCode, floorNumber: floorNumber };
    const floorRecord = await this.floorSchema.findOne(query as FilterQuery<IFloorPersistence & Document>);
    const floorResult = FloorMap.toDomain(floorRecord);
    if (floorResult.isSuccess) {
      return floorResult.getValue();
    } else {
      return null;
    }
  }

  public async save(floor: Floor): Promise<Floor> {
    const query = { id: floor.id.toString() };

    const floorDocument = await this.floorSchema.findOne(query);

    try {
      if (floorDocument === null) {
        const rawFloor: any = FloorMap.toPersistence(floor);
        const floorCreated = await this.floorSchema.create(rawFloor);

        const floorResult = FloorMap.toDomain(floorCreated);

        return floorResult.isSuccess ? floorResult.getValue() : null;
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

  public async findById(floorId: string): Promise<Floor | null> {
    const query = { id: floorId };

    const floorDocument = await this.floorSchema.findOne(query);

    const floorResult = FloorMap.toDomain(floorDocument);

    return floorResult.isSuccess ? floorResult.getValue() : null;
  }

  public async findByBuildingCode(buildingCode: string): Promise<Floor[]> {
    const query = { buildingCode: buildingCode };

    const floorDocuments = await this.floorSchema.find(query);

    if (floorDocuments === null) {
      return null;
    }

    const floorResults = floorDocuments.map((floorDocument) => FloorMap.toDomain(floorDocument));

    return floorResults.map((floorResult) => floorResult.getValue());
  }

  public async findByCodes(buildingCode: string, floorNumbers: number[]): Promise<Floor[]> {
    const query = { buildingCode: buildingCode, floorNumber: { $in: floorNumbers } };

    const floorDocuments = await this.floorSchema.find(query);

    if (floorDocuments === null) {
      return null;
    }

    const floorResults = floorDocuments.map((floorDocument) => FloorMap.toDomain(floorDocument));

    return floorResults.map((floorResult) => floorResult.getValue());
  }

  public async findAllFloors(): Promise<Floor[]> {
    const floorRecords = await this.floorSchema.find({});

    return floorRecords.map((record) => FloorMap.toDomain(record).getValue());
  }
}
