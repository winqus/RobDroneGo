import { Service } from 'typedi';
import { UniqueEntityID } from '../../core/domain/UniqueEntityID';
import { BaseFakeRepo } from '../../core/tests/BaseFakeRepo';
import { Robot } from '../../domain/Robot/robot';
import IRobotRepo from '../../services/IRepos/IRobotRepo';

@Service()
export default class FakeRobotRepo extends BaseFakeRepo<Robot> implements IRobotRepo {
  public async exists(robot: Robot): Promise<boolean> {
    return this._items.some((r) => this.compareFakeItems(r, robot));
  }

  public async save(robot: Robot): Promise<Robot> {
    this.addFakeItem(robot);

    return robot;
  }

  public getItems(): Robot[] {
    return this._items;
  }

  public async findById(id: string): Promise<Robot> {
    const robot = this._items.find((r) => r.id.equals(new UniqueEntityID(id)));

    return robot || null;
  }

  public async findByCode(code: string): Promise<Robot> {
    const robot = this._items.find((r) => r.code.value === code);

    return robot || null;
  }

  public async findAvailable(): Promise<Robot[]> {
    const robots = this._items.filter((r) => r.available === true);

    return robots || null;
  }

  public async findAll(): Promise<Robot[]> {
    return this._items;
  }

  public async findByType(robotTypeName: string): Promise<Robot[]> {
    const robots = this._items.filter((r) => r.type.value === robotTypeName);

    return robots || null;
  }

  compareFakeItems(a: Robot, b: Robot): boolean {
    return a.id.equals(b.id);
  }
}
