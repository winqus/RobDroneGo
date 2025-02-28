import { Service } from 'typedi';
import { UniqueEntityID } from '../../core/domain/UniqueEntityID';
import { BaseFakeRepo } from '../../core/tests/BaseFakeRepo';
import { RobotType } from '../../domain/RobotType/robotType';
import IRobotTypeRepo from '../../services/IRepos/IRobotTypeRepo';

@Service()
export default class FakeRobotTypeRepo extends BaseFakeRepo<RobotType> implements IRobotTypeRepo {
  public async exists(robotType: RobotType): Promise<boolean> {
    return this._items.some((r) => this.compareFakeItems(r, robotType));
  }

  public async save(robotType: RobotType): Promise<RobotType> {
    this.addFakeItem(robotType);

    return robotType;
  }

  public getItems(): RobotType[] {
    return this._items;
  }

  public async findById(id: string): Promise<RobotType> {
    const robotType = this._items.find((r) => r.id.equals(new UniqueEntityID(id)));

    return robotType || null;
  }

  public async findByName(name: string): Promise<RobotType> {
    const robotType = this._items.find((r) => r.name.value === name);

    return robotType || null;
  }

  public async findAllRobotTypes(): Promise<RobotType[]> {
    return this._items;
  }

  public async findByBrandAndModel(brand: string, model: string): Promise<RobotType> {
    const robotType = this._items.find((r) => r.brand.value === brand && r.model.value === model);

    return robotType || null;
  }

  public async findByMultiple(
    name?: string,
    brand?: string,
    model?: string,
    taskTypes?: string[],
  ): Promise<RobotType[]> {
    let robotTypes = this._items;

    if (name) {
      robotTypes = robotTypes.filter((r) => r.name.value === name);
    }

    if (brand) {
      robotTypes = robotTypes.filter((r) => r.brand.value === brand);
    }

    if (model) {
      robotTypes = robotTypes.filter((r) => r.model.value === model);
    }

    if (taskTypes) {
      robotTypes = robotTypes.filter((r) => r.typesOfTasks.some((t) => taskTypes.includes(t.type.toString())));
    }

    return robotTypes;
  }

  compareFakeItems(a: RobotType, b: RobotType): boolean {
    return a.id.equals(b.id);
  }
}
