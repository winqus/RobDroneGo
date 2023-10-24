import { Repo } from '../../core/infra/Repo';
import { Building } from '../../domain/Building/building';

export default interface IBuildingRepo extends Repo<Building> {
  save(building: Building): Promise<Building>;

  findById(buildingId: string): Promise<Building>;

  //findByIds (buildingsIds: BuildingId[]): Promise<Building[]>;
  //saveCollection (buildings: Building[]): Promise<Building[]>;
  //removeByBuildingIds (buildings: BuildingId[]): Promise<any>
}
