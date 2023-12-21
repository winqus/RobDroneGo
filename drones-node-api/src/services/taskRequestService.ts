import http, { request } from 'http';
import querystring from 'querystring';
import { Inject, Service } from 'typedi';
import config from '../../config';
import { Result } from '../core/logic/Result';
import { DeliveryTask } from '../domain/DeliveryTask/deliveryTask';
import { NavigationData } from '../domain/TaskRequest/navigationData';
import { TaskRequest } from '../domain/TaskRequest/taskRequest';
import { TaskStatus } from '../domain/TaskRequest/taskStatus';
import { ITaskRequestDTO } from '../dto/ITaskRequestDTO';
import { DeliveryTaskMap } from '../mappers/deliveryTaskMap';
import { NavigationDataMap } from '../mappers/navigationDataMap';
import { SurveillanceTaskMap } from '../mappers/surveillanceTaskMap';
import { TaskRequestMap } from '../mappers/taskRequestMap';
import ITaskRequestService from './IServices/ITaskRequestService';

@Service()
export default class TaskRequestService implements ITaskRequestService {
  constructor(
    @Inject(config.repos.taskRequest.name) private taskRequestRepo,
    @Inject(config.repos.room.name) private roomRepo,
    @Inject(config.repos.floor.name) private floorRepo,
  ) {}

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

      const taskRequest = taskRequestOrError.getValue();

      try {
        if (
          taskRequest.status === TaskStatus.Approved &&
          taskRequest.task instanceof DeliveryTask &&
          taskRequest.navigationData == null
        ) {
          let room = await this.roomRepo.findById(taskRequest.task.pickUpRoomId);
          let floor = await this.floorRepo.findById(room.floorId);

          const origin_building_code = floor.buildingCode.value;
          const origin_floor_number = floor.floorNumber;

          room = await this.roomRepo.findById(taskRequest.task.deliveryRoomId);
          floor = await this.floorRepo.findById(room.floorId);

          const destination_building_code = floor.buildingCode.value;
          const destination_floor_number = floor.floorNumber;

          const data = {
            origin_building_code: origin_building_code,
            origin_floor_number: origin_floor_number,
            origin_room: taskRequest.task.pickUpRoomId.toString(),
            destination_building_code: destination_building_code,
            destination_floor_number: destination_floor_number,
            destination_room: taskRequest.task.deliveryRoomId.toString(),
            minimize_elevator_uses: true,
            minimize_building_count: false,
          };

          const path = `${config.api.prefix}/planning/calculate-cells`;

          const queryString = querystring.stringify(data as any);

          const options = {
            hostname: '127.0.0.1',
            port: config.port,
            path: `${path}?${queryString}`,
            method: 'GET',
          };

          const request = http.request(options, (response) => {
            if (response.headers['content-type']) {
            }

            let body = '';

            response.on('data', (chunk) => {
              body += chunk;
            });

            response.on('end', () => {
              const navigation = NavigationDataMap.toDomain(JSON.parse(body));
              taskRequest.navigationData = navigation.getValue();
              this.taskRequestRepo.save(taskRequest);
            });
          });
          request.on('error', (error) => {
            console.error(`Problem with request: ${error.message}`);
          });

          request.end();
        }
      } catch (ignored) {}

      await this.taskRequestRepo.save(taskRequest);

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
