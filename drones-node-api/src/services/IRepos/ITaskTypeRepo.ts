import { Repo } from '../../core/infra/Repo';
import { TaskType } from '../../domain/TaskType/taskType';

export default interface ITaskTypeRepo extends Repo<TaskType> {
  save(taskType: TaskType): Promise<TaskType>;

  findById(taskTypeId: string): Promise<TaskType>;

  findByType(taskTypeType: string): Promise<TaskType>;
}
