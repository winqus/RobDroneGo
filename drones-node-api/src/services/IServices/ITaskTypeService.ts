import { UniqueEntityID } from '../../core/domain/UniqueEntityID';
import { Result } from '../../core/logic/Result';
import ITaskTypeDTO from '../../dto/ITaskTypeDTO';

export default interface ITaskTypeService {
  createTaskType(taskTypeDTO: ITaskTypeDTO): Promise<Result<ITaskTypeDTO>>;
  getTaskType(taskTypeDTO: ITaskTypeDTO): Promise<Result<ITaskTypeDTO>>;
}
