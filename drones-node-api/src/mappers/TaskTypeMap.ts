import { UniqueEntityID } from '../core/domain/UniqueEntityID';
import { Mapper } from '../core/infra/Mapper';
import { ITaskTypePersistence } from '../dataschema/ITaskTypePersistence';
import { TaskType } from '../domain/TaskType/taskType';
import { Types } from '../domain/TaskType/type';
import ITaskTypeDTO from '../dto/ITaskTypeDTO';

export class TaskTypeMap extends Mapper<TaskType> {
  public static toDTO(taskType: TaskType): ITaskTypeDTO {
    const typeString: string = Types[taskType.type];

    return {
      id: taskType.id.toString(),
      type: typeString,
    } as ITaskTypeDTO;
  }

  public static toDomain(raw: any): TaskType {
    const type = Types[raw.type as keyof typeof Types];
    const taskTypeOrError = TaskType.create(
      {
        type,
      },
      new UniqueEntityID(raw.id),
    );

    taskTypeOrError.isFailure ? console.log(taskTypeOrError.error) : '';

    return taskTypeOrError.isSuccess ? taskTypeOrError.getValue() : null;
  }

  public static toPersistence(taskType: TaskType): ITaskTypePersistence {
    const typeString: string = Types[taskType.type];

    return {
      id: taskType.id.toString(),
      type: typeString,
    };
  }
}
