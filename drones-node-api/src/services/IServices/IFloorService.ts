import { Result } from '../../core/logic/Result';
import IFloorDTO from '../../dto/IFloorDTO';
import IMapDTO from '../../dto/IMapDTO';

export default interface IFloorService {
  createFloor(floorDTO: IFloorDTO): Promise<Result<IFloorDTO>>;

  getAllFloors(): Promise<Result<IFloorDTO[]>>;

  getFloorsByBuildingCode(buildingCode: string): Promise<Result<IFloorDTO[]>>;

  getFloorsServedByElevator(buildingCode: string): Promise<Result<IFloorDTO[]>>;

  updateFloor(updatedFloorDTO: IFloorDTO): Promise<Result<IFloorDTO>>;

  partialUpdateFloor(updatedFloorDTO: Partial<IFloorDTO>): Promise<Result<IFloorDTO>>;

  loadMap(
    buildingCode: string,
    floorNumber: number,
    map: { size: { width: number; height: number }; map: number[][] },
  ): Promise<Result<IFloorDTO>>;

  getMap(buildingCode: string, floorNumber: number): Promise<Result<IMapDTO>>;
}
