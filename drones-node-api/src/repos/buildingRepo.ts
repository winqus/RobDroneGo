import { Inject, Service } from 'typedi';

import { BuildingMap } from '../mappers/BuildingMap';
import { ElevatorMap } from '../mappers/ElevatorMap';

import { Document, FilterQuery, Model } from 'mongoose';
import { UniqueEntityID } from '../core/domain/UniqueEntityID';
import { IBuildingPersistence } from '../dataschema/IBuildingPersistence';
import { Building } from '../domain/Building/building';
import IBuildingRepo from '../services/IRepos/IBuildingRepo';

@Service()
export default class BuildingRepo implements IBuildingRepo {
  static buildingSchema: any;
  constructor(@Inject('buildingSchema') private buildingSchema: Model<IBuildingPersistence & Document>) {}

  public async exists(building: Building): Promise<boolean> {
    const idX = building.id instanceof UniqueEntityID ? (<UniqueEntityID>building.id).toValue() : building.id;

    const query = { domainId: idX };
    const buildingDocument = await this.buildingSchema.findOne(query as FilterQuery<IBuildingPersistence & Document>);

    return Boolean(buildingDocument) === true;
  }

  public async findById(buildingId: string): Promise<Building> {
    const query = { id: buildingId };
    const buildingRecord = await this.buildingSchema.findOne(query as FilterQuery<IBuildingPersistence & Document>);

    if (buildingRecord != null) {
      return BuildingMap.toDomain(buildingRecord);
    } else {
      return null;
    }
  }

  public async findByCode(buildingCode: string): Promise<Building> {
    const query = { code: buildingCode };
    const buildingRecord = await this.buildingSchema.findOne(query as FilterQuery<IBuildingPersistence & Document>);
    if (buildingRecord != null) {
      return BuildingMap.toDomain(buildingRecord);
    } else {
      return null;
    }
  }

  public async findAllBuildings(): Promise<Building[]> {
    const buildingRecords = await this.buildingSchema.find({});

    return buildingRecords.map((record) => BuildingMap.toDomain(record));
  }

  public async save(building: Building): Promise<Building> {
    const query = { id: building.id.toString() };

    const buildingDocument = await this.buildingSchema.findOne(query);

    try {
      if (buildingDocument === null) {
        const rawBuilding: any = BuildingMap.toPersistence(building);
        const buildingCreated = await this.buildingSchema.create(rawBuilding);

        return BuildingMap.toDomain(buildingCreated);
      } else {
        buildingDocument.name = building.name.value;
        buildingDocument.code = building.code.value;
        buildingDocument.description = building.description.value;
        buildingDocument.floorSizeLength = building.floorSize.value.length;
        buildingDocument.floorSizeWidth = building.floorSize.value.width;
        buildingDocument.elevator = building.elevator ? ElevatorMap.toPersistence(building.elevator) : null;

        await buildingDocument.save();

        return building;
      }
    } catch (error) {
      throw error;
    }
  }
}
