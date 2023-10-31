import { Inject, Service } from 'typedi';
import config from '../../config';
import { UniqueEntityID } from '../core/domain/UniqueEntityID';
import { Result } from '../core/logic/Result';
import { TaskType } from '../domain/TaskType/taskType';
import { Types } from '../domain/TaskType/type';
import ITaskTypeDTO from '../dto/ITaskTypeDTO';
import { TaskTypeMap } from '../mappers/TaskTypeMap';
import ITaskTypeRepo from './IRepos/ITaskTypeRepo';
import ITaskTypeService from './IServices/ITaskTypeService';

@Service()
export default class TaskTypeService implements ITaskTypeService {
  constructor(@Inject(config.repos.taskType.name) private taskTypeRepo: ITaskTypeRepo) {}

  public async createTaskType(taskTypeDTO: ITaskTypeDTO): Promise<Result<ITaskTypeDTO>> {
    try {
      const typeResult = Types[taskTypeDTO.type as keyof typeof Types];

      if (typeResult === undefined) {
        return Result.fail<ITaskTypeDTO>('Invalid task type');
      }

      const taskTypeOrError = TaskType.create({
        type: typeResult,
      });

      if (taskTypeOrError.isFailure) {
        return Result.fail<ITaskTypeDTO>(taskTypeOrError.errorValue().toString());
      }

      const taskTypeResult = taskTypeOrError.getValue();
      if ((await this.taskTypeRepo.findByType(taskTypeResult.type.toString())) !== null) {
        return Result.fail<ITaskTypeDTO>('Task Type already exists');
      }

      try {
        await this.taskTypeRepo.save(taskTypeResult);
      } catch (error) {
        return Result.fail<ITaskTypeDTO>(error);
      }

      const taskTypeDTOResult = TaskTypeMap.toDTO(taskTypeResult);

      return Result.ok<ITaskTypeDTO>(taskTypeDTOResult);
    } catch (error) {
      throw error;
    }
  }

  public async getTaskType(taskTypeDTO: ITaskTypeDTO): Promise<Result<ITaskTypeDTO>> {
    try {
      const taskType = await this.taskTypeRepo.findByType(taskTypeDTO.type);

      if (!taskType) {
        return Result.fail<ITaskTypeDTO>('TaskType not found');
      }

      const taskTypeDTOResult = TaskTypeMap.toDTO(taskType);

      return Result.ok<ITaskTypeDTO>(taskTypeDTOResult);
    } catch (error) {
      return Result.fail<ITaskTypeDTO>(error);
    }
  }
}
