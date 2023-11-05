import { Repo } from '../../core/infra/Repo';
import { Floor } from '../../domain/Floor/floor';

export default interface IFloorRepo extends Repo<Floor> {
  save(floor: Floor): Promise<Floor>;
  findAllFloors(): Promise<Floor[]>;
  findByBuildingCode(codeBuilding: string): Promise<Floor[]>;

  findByCode(buildingCode: string, floorNumber: number): Promise<Floor>;

  findById(floorId: Floor | string): Promise<Floor>;

  findByBuildingCode(buildingCode: string): Promise<Floor[]>;

  findByCodes(buildingCode: string, floorNumbers: number[]): Promise<Floor[]>;
}
