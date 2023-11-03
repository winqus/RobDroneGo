import { Document, FilterQuery, Model } from 'mongoose';
import { Inject, Service } from 'typedi';
import { UniqueEntityID } from '../core/domain/UniqueEntityID';
import IRobotPersistence from '../dataschema/IRobotPersistence';
import { Robot } from '../domain/Robot/robot';
import { RobotMap } from '../mappers/RobotMap';
import IRobotRepo from '../services/IRepos/IRobotRepo';

@Service()
export default class RobotRepo implements IRobotRepo {
  constructor(@Inject('robotSchema') private robotSchema: Model<IRobotPersistence & Document>) {}

  public async exists(robot: Robot): Promise<boolean> {
    const idX = robot.id instanceof UniqueEntityID ? (<UniqueEntityID>robot.id).toValue() : robot.id;

    const query = { domainId: idX };
    const robotDocument = await this.robotSchema.findOne(query as FilterQuery<IRobotPersistence & Document>);

    return Boolean(robotDocument) === true;
  }

  public async save(robot: Robot): Promise<Robot> {
    const query = { id: robot.id.toString() };

    const robotDocument = await this.robotSchema.findOne(query);

    try {
      if (robotDocument === null) {
        const rawRobot: any = RobotMap.toPersistence(robot);
        const robotCreated = await this.robotSchema.create(rawRobot);

        return RobotMap.toDomain(robotCreated) ? RobotMap.toDomain(robotCreated).getValue() : null;
      } else {
        robotDocument.code = robot.code.value;
        robotDocument.description = robot.description.value;
        robotDocument.nickname = robot.nickname.value;
        robotDocument.serialNumber = robot.serialNumber.value;
        robotDocument.available = robot.available;
        robotDocument.type = robot.type.value;

        await robotDocument.save();

        return robot;
      }
    } catch (error) {
      throw error;
    }
  }

  public async findById(robotId: string): Promise<Robot | null> {
    const query = { id: robotId };
    const robotRecord = await this.robotSchema.findOne(query as FilterQuery<IRobotPersistence & Document>);
    if (!robotRecord) {
      return null;
    }
    const robotResult = RobotMap.toDomain(robotRecord);
    if (robotResult.isSuccess) {
      return robotResult.getValue();
    } else {
      return null;
    }
  }

  public async findByCode(robotCode: string): Promise<Robot | null> {
    const query = { code: robotCode };
    const robotRecord = await this.robotSchema.findOne(query as FilterQuery<IRobotPersistence & Document>);

    if (!robotRecord) {
      return null;
    }
    const robotResult = RobotMap.toDomain(robotRecord);
    if (robotResult.isSuccess) {
      return robotResult.getValue();
    } else {
      return null;
    }
  }
}
