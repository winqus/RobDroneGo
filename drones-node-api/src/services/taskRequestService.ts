import { Inject, Service } from 'typedi';
import config from '../../config';
import { Result } from '../core/logic/Result';
import { DeliveryTask } from '../domain/DeliveryTask/deliveryTask';
import { SurveillanceTask } from '../domain/SurveillanceTask/surveillanceTask';
import { NavigationData } from '../domain/TaskRequest/navigationData';
import { TaskRequest } from '../domain/TaskRequest/taskRequest';
import { TaskStatus } from '../domain/TaskRequest/taskStatus';
import { IDeliveryTaskDTO } from '../dto/IDeliveryTaskDTO';
import { ISurveillanceTaskDTO } from '../dto/ISurveillanceTaskDTO';
import { ITaskRequestDTO } from '../dto/ITaskRequestDTO';
import { DeliveryTaskMap } from '../mappers/deliveryTaskMap';
import { SurveillanceTaskMap } from '../mappers/surveillanceTaskMap';
import { TaskRequestMap } from '../mappers/taskRequestMap';
import { DeliveryTask as DeliveryTaskSchema } from '../persistence/schemas/deliveryTaskSchema';
import { SurveillanceTask as SurveillanceTaskSchema } from '../persistence/schemas/surveillanceTaskSchema';
import ITaskRequestService from './IServices/ITaskRequestService';

@Service()
export default class TaskRequestService implements ITaskRequestService {
  constructor(@Inject(config.repos.taskRequest.name) private taskRequestRepo) {}

  async addNavigationData(
    taskRequestId: string,
    navigationData: {
      floorsPaths: {
        fromBuilding: string;
        fromFloorNumber: string;
        toBuilding: string;
        toFloorNumber: string;
        type: string;
      }[];
      mapPathCount: number;
      mapPaths: { buildingCode: string; cost: number; floorNumber: number; path: { col: number; row: number }[] }[];
    },
  ): Promise<Result<ITaskRequestDTO>> {
    try {
      const request = await this.taskRequestRepo.getById(taskRequestId);

      if (request === null) {
        return Result.fail<ITaskRequestDTO>('TaskRequest with provided id does not exist.');
      }

      const navDataObject: Result<NavigationData> = NavigationData.create({
        floorsPaths: navigationData.floorsPaths,
        mapPathCount: navigationData.mapPathCount,
        mapPaths: navigationData.mapPaths.map((path) => ({
          buildingCode: path.buildingCode,
          cost: path.cost,
          floorNumber: path.floorNumber,
          path: path.path.map((point) => ({ col: point.col, row: point.row })),
        })),
      });

      if (navDataObject.isFailure) {
        return Result.fail<ITaskRequestDTO>(navDataObject.errorValue().toString());
      }

      request.navigationData = navDataObject.getValue();

      this.taskRequestRepo.save(request);

      const taskRequestDTOResult = TaskRequestMap.toDTO(request) as ITaskRequestDTO;

      return Result.ok<ITaskRequestDTO>(taskRequestDTOResult);
    } catch (error) {
      return Result.fail<ITaskRequestDTO>(error);
    }
  }

  async changeState(updatedTaskRequestDTO: Partial<ITaskRequestDTO>): Promise<Result<ITaskRequestDTO>> {
    try {
      const existingTaskRequest = await this.taskRequestRepo.getById(updatedTaskRequestDTO.id);
      if (!existingTaskRequest) {
        return Result.fail<ITaskRequestDTO>('TaskRequest does not exist.');
      }

      if (
        existingTaskRequest.status ===
        Object.values(TaskStatus).indexOf((updatedTaskRequestDTO.status as unknown) as TaskStatus)
      ) {
        return Result.fail<ITaskRequestDTO>('TaskRequest is already in the requested state.');
      }

      const updatedTaskRequestData = {
        id: existingTaskRequest.id.toString(),
        status: updatedTaskRequestDTO.status,
        requesterEmail: existingTaskRequest.requesterEmail,
        task:
          existingTaskRequest.task instanceof DeliveryTask
            ? DeliveryTaskMap.toDTO(existingTaskRequest.task)
            : SurveillanceTaskMap.toDTO(existingTaskRequest.task),
        requestCreatedDateTime: existingTaskRequest.requestCreatedDateTime.toString(),
        navigationData: existingTaskRequest.navigationData,
      };

      const taskRequestOrError = TaskRequestMap.toDomain(updatedTaskRequestData);

      if (taskRequestOrError.isFailure) {
        return Result.fail<ITaskRequestDTO>(taskRequestOrError.errorValue().toString());
      }

      await this.taskRequestRepo.save(taskRequestOrError.getValue());

      const taskRequestDTOResult = TaskRequestMap.toDTO(taskRequestOrError.getValue()) as ITaskRequestDTO;

      return Result.ok<ITaskRequestDTO>(taskRequestDTOResult);
    } catch (error) {
      return Result.fail<ITaskRequestDTO>(error);
    }
  }

  public async create(taskRequestDTO: ITaskRequestDTO): Promise<Result<ITaskRequestDTO>> {
    try {
      taskRequestDTO.status = Object.keys(TaskStatus).at(Object.values(TaskStatus).indexOf(TaskStatus.Pending));
      taskRequestDTO.requestCreatedDateTime = new Date().toISOString();

      const taskRequestOrError = TaskRequestMap.toDomain(taskRequestDTO);

      if (taskRequestOrError.isFailure) {
        return Result.fail<ITaskRequestDTO>(taskRequestOrError.errorValue().toString());
      }

      const taskRequestResult = taskRequestOrError.getValue();

      try {
        await this.taskRequestRepo.save(taskRequestResult);
      } catch (error) {
        return Result.fail<ITaskRequestDTO>(error);
      }

      const taskRequestDTOResult = TaskRequestMap.toDTO(taskRequestResult) as ITaskRequestDTO;

      return Result.ok<ITaskRequestDTO>(taskRequestDTOResult);
    } catch (error) {
      return Result.fail<ITaskRequestDTO>(error);
    }
  }

  public async getAll(): Promise<Result<ITaskRequestDTO[]>> {
    try {
      const taskRequests = await this.taskRequestRepo.getAll();

      const taskRequestsDTOs: ITaskRequestDTO[] = taskRequests.map((taskRequest: TaskRequest) => {
        return TaskRequestMap.toDTO(taskRequest);
      });

      return Result.ok<ITaskRequestDTO[]>(taskRequestsDTOs);
    } catch (error) {
      return Result.fail<ITaskRequestDTO[]>(error);
    }
  }

  public async getById(taskRequestId: string): Promise<Result<ITaskRequestDTO>> {
    try {
      const taskRequest = await this.taskRequestRepo.getById(taskRequestId);

      if (!taskRequest) {
        return Result.fail<ITaskRequestDTO>('TaskRequest not found');
      }

      const taskRequestDTO = TaskRequestMap.toDTO(taskRequest);

      return Result.ok<ITaskRequestDTO>(taskRequestDTO);
    } catch (error) {
      return Result.fail<ITaskRequestDTO>(error);
    }
  }
}
