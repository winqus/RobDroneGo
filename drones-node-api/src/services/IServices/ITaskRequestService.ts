import { Result } from '../../core/logic/Result';
import { ITaskRequestDTO } from '../../dto/ITaskRequestDTO';

export default interface ITaskRequestService {
  create(TaskDto: ITaskRequestDTO): Promise<Result<ITaskRequestDTO>>;

  changeState(updatedTaskRequestDTO: Partial<ITaskRequestDTO>): Promise<Result<ITaskRequestDTO>>;

  addNavigationData(
    taskRequestId: string,
    navigationData: {
      floorsPaths: {
        fromBuilding: string;
        fromFloorNumber: string;
        toBuilding: string;
        toFloorNumber: string;
        type: string;
      }[];
      mapPathCount: number;
      mapPaths: { buildingCode: string; cost: number; floorNumber: number; path: { col: number; row: number }[] }[];
    },
  ): Promise<Result<ITaskRequestDTO>>;

  getAll(): Promise<Result<ITaskRequestDTO[]>>;

  getById(id: string): Promise<Result<ITaskRequestDTO>>;
}
