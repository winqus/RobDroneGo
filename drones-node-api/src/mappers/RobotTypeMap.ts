import { Container } from 'typedi';
import config from '../../config';
import { UniqueEntityID } from '../core/domain/UniqueEntityID';
import { Mapper } from '../core/infra/Mapper';
import { Brand } from '../domain/RobotType/ValueObjects/brand';
import { Model } from '../domain/RobotType/ValueObjects/model';
import { Name } from '../domain/RobotType/ValueObjects/name';
import { RobotType } from '../domain/RobotType/robotType';
import IRobotTypeDTO from '../dto/IRobotTypeDTO';
import ITaskTypeRepo from '../services/IRepos/ITaskTypeRepo';
import { TaskTypeMap } from './TaskTypeMap';

export class RobotTypeMap extends Mapper<RobotType> {
  public static toDTO(robotType: RobotType): IRobotTypeDTO {
    return {
      id: robotType.id.toString(),
      name: robotType.name.value,
      brand: robotType.brand.value,
      model: robotType.model.value,
      typesOfTasks: robotType.typesOfTasks.map((taskType) => TaskTypeMap.toDTO(taskType).type),
    } as IRobotTypeDTO;
  }

  public static toDomain(raw: any): RobotType {
    const nameOrError = Name.create(raw.name);
    const brandOrError = Brand.create(raw.brand);
    const modelOrError = Model.create(raw.model);
    const typesOfTasksOrError = raw.typesOfTasks.map((taskType) => TaskTypeMap.toDomain({ type: taskType }));

    const robotTypeOrError = RobotType.create(
      {
        name: nameOrError.getValue(),
        brand: brandOrError.getValue(),
        model: modelOrError.getValue(),
        typesOfTasks: typesOfTasksOrError,
      },
      new UniqueEntityID(raw.id),
    );

    robotTypeOrError.isFailure ? console.log(robotTypeOrError.error) : '';

    return robotTypeOrError.isSuccess ? robotTypeOrError.getValue() : null;
  }

  public static toPersistence(robotType: RobotType): any {
    return {
      id: robotType.id.toString(),
      name: robotType.name.value,
      brand: robotType.brand.value,
      model: robotType.model.value,
      typesOfTasks: robotType.typesOfTasks.map((taskType) => TaskTypeMap.toDTO(taskType).type),
    };
  }
}
