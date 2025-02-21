import { Model } from 'mongoose';
import { Service } from 'typedi';
import { UniqueEntityID } from '../../core/domain/UniqueEntityID';
import { IFloorPersistence } from '../../dataschema/IFloorPersistence';
import { Floor } from '../../domain/Floor/floor';
import IFloorRepo from '../../services/IRepos/IFloorRepo';

@Service()
export default class FakeFloorRepo implements IFloorRepo {
  getFloorOfRoom(roomName: string): Promise<Floor> {
    throw new Error('Method not implemented.');
  }

  public async findAllFloors(): Promise<Floor[]> {
    return this._items;
  }

  private _items: Floor[] = [];

  public async exists(floor: Floor): Promise<boolean> {
    return this._items.some((f) => this.compareFakeItems(f, floor));
  }

  public async save(floor: Floor): Promise<Floor> {
    this.addFakeItem(floor);

    return floor;
  }

  public async findByCode(buildingCode: string, floorNumber: number): Promise<Floor> {
    const floor = this._items.find((f) => f.buildingCode.value === buildingCode && f.floorNumber === floorNumber);

    return floor || null;
  }

  public async findById(id: string): Promise<Floor> {
    const floor = this._items.find((f) => f.id.equals(new UniqueEntityID(id)));

    return floor || null;
  }

  public async findByBuildingCode(buildingCode: string): Promise<Floor[]> {
    return this._items.filter((f) => f.buildingCode.value === buildingCode);
  }

  public async findByCodes(buildingCode: string, floorNumbers: number[]): Promise<Floor[]> {
    return this._items.filter((f) => f.buildingCode.value === buildingCode && floorNumbers.includes(f.floorNumber));
  }

  compareFakeItems(a: Floor, b: Floor): boolean {
    return a.id.equals(b.id);
  }

  public getItems(): Floor[] {
    return this._items;
  }

  private addFakeItem(floor: Floor) {
    this._items.push(floor);
  }
}
