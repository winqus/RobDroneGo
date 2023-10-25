import { UniqueEntityID } from '../../core/domain/UniqueEntityID';
import { Result } from '../../core/logic/Result';
import IBuildingDTO from '../../dto/IBuildingDTO';

export default interface IBuildingService {
  createBuilding(buildingDTO: IBuildingDTO): Promise<Result<IBuildingDTO>>;

  updateBuilding(buildingDTO: IBuildingDTO): Promise<Result<IBuildingDTO>>;

  getAllBuildings(): Promise<Result<IBuildingDTO[]>>;

  getBuildingByCode(buildingCode: string): Promise<Result<IBuildingDTO>>;
  // getBuilding(buildingId: string): Promise<Result<IBuildingDTO>>;
}
