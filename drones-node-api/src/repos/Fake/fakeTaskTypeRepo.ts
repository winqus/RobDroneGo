import { Service } from 'typedi';
import { TaskType } from '../../domain/TaskType/taskType';
import ITaskTypeRepo from '../../services/IRepos/ITaskTypeRepo';

@Service()
export default class FakeTaskTypeRepo implements ITaskTypeRepo {
  private _items: TaskType[] = [];

  public async exists(taskType: TaskType): Promise<boolean> {
    return this._items.some((t) => t.id.equals(taskType.id));
  }

  public async save(taskType: TaskType): Promise<TaskType> {
    this.addFakeItem(taskType);

    return taskType;
  }

  public async findById(taskTypeId: string): Promise<TaskType> {
    return this._items.find((t) => t.id.toString() === taskTypeId) || null;
  }

  public async findByType(taskTypeType: string): Promise<TaskType> {
    return this._items.find((t) => t.type.toString() === taskTypeType) || null;
  }

  compareFakeItems(a: TaskType, b: TaskType): boolean {
    return a.id.equals(b.id);
  }

  public getItems(): TaskType[] {
    return this._items;
  }

  private addFakeItem(taskType: TaskType) {
    this._items.push(taskType);
  }
}
