import { UniqueEntityID } from '../core/domain/UniqueEntityID';
import { Mapper } from '../core/infra/Mapper';
import { Result } from '../core/logic/Result';
import { ITaskRequestPersistence } from '../dataschema/ITaskRequestPersistence';
import { DeliveryTask } from '../domain/DeliveryTask/deliveryTask';
import { NavigationData } from '../domain/TaskRequest/navigationData';
import { TaskRequest } from '../domain/TaskRequest/taskRequest';
import { TaskStatus } from '../domain/TaskRequest/taskStatus';
import { ITaskRequestDTO } from '../dto/ITaskRequestDTO';
import { DeliveryTaskMap } from './deliveryTaskMap';
import { NavigationDataMap } from './navigationDataMap';
import { SurveillanceTaskMap } from './surveillanceTaskMap';

export class TaskRequestMap extends Mapper<TaskRequest> {
  public static toDTO(taskRequest: TaskRequest): ITaskRequestDTO {
    return {
      id: taskRequest.id.toString(),
      status: Object.keys(TaskStatus).at(Object.values(TaskStatus).indexOf(TaskStatus.Pending)),
      requesterEmail: taskRequest.requesterEmail,
      task:
        taskRequest.task instanceof DeliveryTask
          ? DeliveryTaskMap.toDTO(taskRequest.task)
          : SurveillanceTaskMap.toDTO(taskRequest.task),
      requestCreatedDateTime: taskRequest.requestCreatedDateTime.toDateString(),
      navigationData: taskRequest.navigationData ? NavigationDataMap.toDTO(taskRequest.navigationData) : null,
    } as ITaskRequestDTO;
  }

  public static toDomain(raw: any): Result<TaskRequest> {
    let taskOrError;
    if (raw.task.pickUpRoomId) {
      taskOrError = DeliveryTaskMap.toDomain(raw.task);
    } else {
      taskOrError = SurveillanceTaskMap.toDomain(raw.task);
    }

    const taskRequestOrError = TaskRequest.create(
      {
        requesterEmail: raw.requesterEmail,
        status: Object.values(TaskStatus).indexOf((raw.status as unknown) as TaskStatus),
        task: taskOrError.getValue(),
        requestCreatedDateTime: new Date(raw.requestCreatedDateTime),
        navigationData: raw.navigationData ? NavigationData.create(raw.navigationData).getValue() : null,
      },
      new UniqueEntityID(raw.id),
    );

    taskRequestOrError.isFailure ? console.log(taskRequestOrError.error) : '';

    return taskRequestOrError;
  }

  public static toPersistence(taskRequest: TaskRequest): ITaskRequestPersistence {
    return {
      id: taskRequest.id.toString(),
      status: Object.keys(TaskStatus).at(Object.values(TaskStatus).indexOf(TaskStatus.Pending)),
      requesterEmail: taskRequest.requesterEmail,
      task:
        taskRequest.task instanceof DeliveryTask
          ? DeliveryTaskMap.toDTO(taskRequest.task)
          : SurveillanceTaskMap.toDTO(taskRequest.task),
      requestCreatedDateTime: taskRequest.requestCreatedDateTime.toDateString(),
      navigationData: taskRequest.navigationData ? NavigationDataMap.toPersistence(taskRequest.navigationData) : null,
    } as ITaskRequestPersistence;
  }
}
