import { Repo } from '../../core/infra/Repo';
import { TaskRequest } from '../../domain/TaskRequest/taskRequest';

export default interface ITaskRequestRepo extends Repo<TaskRequest> {
  getAll(): Promise<TaskRequest[]>;

  getById(id: string): Promise<TaskRequest>;
}
