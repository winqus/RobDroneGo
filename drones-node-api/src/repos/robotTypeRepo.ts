import { Document, FilterQuery, Model } from 'mongoose';
import { Inject, Service } from 'typedi';
import { UniqueEntityID } from '../core/domain/UniqueEntityID';

import { IRobotTypePersistence } from '../dataschema/IRobotTypePersistence';
import { RobotType } from '../domain/RobotType/robotType';
import { RobotTypeMap } from '../mappers/RobotTypeMap';
import IRobotTypeRepo from '../services/IRepos/IRobotTypeRepo';

@Service()
export default class RobotTypeRepo implements IRobotTypeRepo {
  constructor(@Inject('robotTypeSchema') private robotTypeSchema: Model<IRobotTypePersistence & Document>) {}

  public async exists(robotType: RobotType): Promise<boolean> {
    const idX = robotType.id instanceof UniqueEntityID ? (<UniqueEntityID>robotType.id).toValue() : robotType.id;

    const query = { domainId: idX };
    const robotTypeDocument = await this.robotTypeSchema.findOne(
      query as FilterQuery<IRobotTypePersistence & Document>,
    );

    return Boolean(robotTypeDocument) === true;
  }

  public async save(robotType: RobotType): Promise<RobotType> {
    const query = { id: robotType.id.toString() };

    const robotTypeDocument = await this.robotTypeSchema.findOne(query);

    try {
      if (robotTypeDocument === null) {
        const rawRobotType: any = RobotTypeMap.toPersistence(robotType);
        const robotTypeCreated = await this.robotTypeSchema.create(rawRobotType);

        //duvidas sobre toDomain de dois maps

        return RobotTypeMap.toDomain(robotTypeCreated);
      } else {
        robotTypeDocument.name = robotType.name.value;
        robotTypeDocument.brand = robotType.brand.value;
        robotTypeDocument.model = robotType.model.value;
        robotTypeDocument.typesOfTasks = robotType.typesOfTasks.map((taskType) => taskType.type.toString());

        await robotTypeDocument.save();

        return robotType;
      }
    } catch (err) {
      throw err;
    }
  }

  public async findById(robotTypeId: string): Promise<RobotType> {
    const query = { id: robotTypeId };
    const robotTypeRecord = await this.robotTypeSchema.findOne(query as FilterQuery<IRobotTypePersistence & Document>);
    if (robotTypeRecord != null) {
      return RobotTypeMap.toDomain(robotTypeRecord);
    } else {
      return null;
    }
  }

  public async findByName(robotTypeName: string): Promise<RobotType> {
    const query = { name: robotTypeName };
    const robotTypeRecord = await this.robotTypeSchema.findOne(query as FilterQuery<IRobotTypePersistence & Document>);
    if (robotTypeRecord != null) {
      return RobotTypeMap.toDomain(robotTypeRecord);
    } else {
      return null;
    }
  }

  public async findByBrandAndModel(robotTypeBrand: string, robotTypeModel: string): Promise<RobotType> {
    const query = { brand: robotTypeBrand, model: robotTypeModel };
    const robotTypeRecord = await this.robotTypeSchema.findOne(query as FilterQuery<IRobotTypePersistence & Document>);
    if (robotTypeRecord != null) {
      return RobotTypeMap.toDomain(robotTypeRecord);
    } else {
      return null;
    }
  }

  public async findByMultiple(
    name?: string,
    brand?: string,
    model?: string,
    taskTypes?: string[],
  ): Promise<RobotType[]> {
    const query: FilterQuery<IRobotTypePersistence & Document> = {};
    if (name) {
      query.name = name;
    }
    if (brand) {
      query.brand = brand;
    }
    if (model) {
      query.model = model;
    }
    if (taskTypes && taskTypes.length) {
      query.typesOfTasks = { $all: taskTypes };
    }

    const robotTypeRecords = await this.robotTypeSchema.find(query);

    return robotTypeRecords.map(RobotTypeMap.toDomain);
  }
}
