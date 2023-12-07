import { UniqueEntityID } from '../../core/domain/UniqueEntityID';
import { Result } from '../../core/logic/Result';
import { TaskRequest } from '../../domain/TaskRequest/taskRequest';

export default interface ITaskRequestService {
  create(TaskDto: ITaskRequestDTO): Promise<Result<TaskRequest>>;

  update(TaskDto: ITaskRequestDTO): Promise<Result<TaskRequest>>;

  getAll(): Promise<Result<TaskRequest[]>>;

  getById(id: UniqueEntityID): Promise<Result<TaskRequest[]>>;
}
