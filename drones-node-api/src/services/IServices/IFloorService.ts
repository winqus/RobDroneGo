import { Result } from '../../core/logic/Result';
import IFloorDTO from '../../dto/IFloorDTO';

export default interface IFloorService {
  createFloor(floorDTO: IFloorDTO): Promise<Result<IFloorDTO>>;
  getAllFloors(): Promise<Result<IFloorDTO[]>>;
  getFloorsByBuildingCode(buildingCode: string): Promise<Result<IFloorDTO[]>>;

  updateFloor(updatedFloorDTO: IFloorDTO): Promise<Result<IFloorDTO>>;

  partialUpdateFloor(updatedFloorDTO: Partial<IFloorDTO>): Promise<Result<IFloorDTO>>;
}
