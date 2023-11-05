import { Repo } from '../../core/infra/Repo';
import { Elevator } from '../../domain/Building/Entities/elevator';
import { Building } from '../../domain/Building/building';

export default interface IBuildingRepo extends Repo<Building> {
  save(building: Building): Promise<Building>;

  findById(buildingId: string): Promise<Building>;

  findByCode(buildingCode: string): Promise<Building>;

  findAllBuildings(): Promise<Building[]>;

  findElevatorsInBuilding(buildingCode: string): Promise<Elevator[]>;
}
