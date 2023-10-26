import { Service } from 'typedi';
import { UniqueEntityID } from '../../core/domain/UniqueEntityID';
import { BaseFakeRepo } from '../../core/tests/BaseFakeRepo';
import { Building } from '../../domain/Building/building';
import IBuildingRepo from '../../services/IRepos/IBuildingRepo';

@Service()
export default class FakeBuildingRepo extends BaseFakeRepo<Building> implements IBuildingRepo {
  public async exists(building: Building): Promise<boolean> {
    return this._items.some((b) => this.compareFakeItems(b, building));
  }

  public async save(building: Building): Promise<Building> {
    this.addFakeItem(building);

    return building;
  }

  public getItems(): Building[] {
    return this._items;
  }

  public async findById(id: string): Promise<Building> {
    const building = this._items.find((b) => b.id.equals(new UniqueEntityID(id)));

    return building || null;
  }

  public async findByCode(code: string): Promise<Building> {
    const building = this._items.find((b) => b.code.value === code);

    return building || null;
  }

  public async findAllBuildings(): Promise<Building[]> {
    return this._items;
  }

  compareFakeItems(a: Building, b: Building): boolean {
    return a.id.equals(b.id);
  }
}
