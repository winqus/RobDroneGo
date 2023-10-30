import { Repo } from '../../core/infra/Repo';
import { Floor } from '../../domain/Floor/floor';

export default interface IFloorRepo extends Repo<Floor> {
  save(floor: Floor): Promise<Floor>;

  findByCode(buildingCode: string, floorNumber: number): Promise<Floor>;

  //findById(floorId: FloorId | string): Promise<Floor>;
  //findByIds (floorsIds: FloorId[]): Promise<Floor[]>;
  //saveCollection (floors: Floor[]): Promise<Floor[]>;
  //removeByFloorsIds (floors: FloorId[]): Promise<any>
}
