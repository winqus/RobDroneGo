import { Inject, Service } from 'typedi';
import config from '../../config';
import { UniqueEntityID } from '../core/domain/UniqueEntityID';
import { Result } from '../core/logic/Result';
import { Name } from '../domain/Building/ValueObjects/name';
import { Brand } from '../domain/RobotType/ValueObjects/brand';
import { Model } from '../domain/RobotType/ValueObjects/model';
import { RobotType } from '../domain/RobotType/robotType';
import { TaskType } from '../domain/TaskType/taskType';
import IRobotTypeDTO from '../dto/IRobotTypeDTO';
import { RobotTypeMap } from '../mappers/RobotTypeMap';
import { TaskTypeMap } from '../mappers/TaskTypeMap';
import IRobotTypeRepo from './IRepos/IRobotTypeRepo';
import IRobotTypeService from './IServices/IRobotTypeService';
import ITaskTypeService from './IServices/ITaskTypeService';

@Service()
export default class RobotTypeService implements IRobotTypeService {
  constructor(
    @Inject(config.repos.robotType.name) private robotTypeRepo: IRobotTypeRepo,
    @Inject(config.services.taskType.name) private taskTypeService: ITaskTypeService,
  ) {}

  public async createRobotType(robotTypeDTO: IRobotTypeDTO): Promise<Result<IRobotTypeDTO>> {
    try {
      const nameResult = await Name.create(robotTypeDTO.name);
      const brandResult = await Brand.create(robotTypeDTO.brand);
      const modelResult = await Model.create(robotTypeDTO.model);
      const typesOfTasksResult = await Promise.all(
        robotTypeDTO.typesOfTasks.map(async (typeString) => {
          let result = await this.taskTypeService.getTaskType({ id: null, type: typeString });
          if (result.isFailure) {
            result = await this.taskTypeService.createTaskType({ id: null, type: typeString });
          }
          if (result.isSuccess) {
            return Result.ok<TaskType>(TaskTypeMap.toDomain(result.getValue()));
          } else {
            return Result.fail<TaskType>(result.getValue());
          }
        }),
      );

      const compositeResult = Result.combine([
        nameResult,
        brandResult,
        modelResult,
        Result.combine(typesOfTasksResult),
      ]);

      if (compositeResult.isFailure) {
        return Result.fail<IRobotTypeDTO>(compositeResult.errorValue().toString());
      }

      const robotTypeOrError = RobotType.create({
        name: nameResult.getValue(),
        brand: brandResult.getValue(),
        model: modelResult.getValue(),
        typesOfTasks: typesOfTasksResult.map((taskTypeResult) => taskTypeResult.getValue()),
      });

      if (robotTypeOrError.isFailure) {
        return Result.fail<IRobotTypeDTO>(robotTypeOrError.errorValue().toString());
      }

      const robotTypeResult = robotTypeOrError.getValue();
      if ((await this.robotTypeRepo.findByName(robotTypeResult.name.toString())) !== null) {
        return Result.fail<IRobotTypeDTO>('Robot Type already exists');
      }

      try {
        await this.robotTypeRepo.save(robotTypeResult);
      } catch (error) {
        return Result.fail<IRobotTypeDTO>(error);
      }

      const robotTypeDTOResult = RobotTypeMap.toDTO(robotTypeResult);

      return Result.ok<IRobotTypeDTO>(robotTypeDTOResult);
    } catch (error) {
      throw error;
    }
  }
}
