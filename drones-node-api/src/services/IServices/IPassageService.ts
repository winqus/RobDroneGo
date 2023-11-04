import { Result } from '../../core/logic/Result';
import IFloorDTO from '../../dto/IFloorDTO';
import IPassageDTO from '../../dto/IPassageDTO';

export default interface IPassageService {
  createPassage(passageDTO: IPassageDTO): Promise<Result<IPassageDTO>>;

  getAllPassages(): Promise<Result<IPassageDTO[]>>;

  getPassagesBetweenBuildings(buildingCode1: string, buildingCode2: string): Promise<Result<IPassageDTO[]>>;

  listFloorsWithPassagesToDifferentBuilding(buildingCode: string): Promise<Result<IFloorDTO[]>>;
}
