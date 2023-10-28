import { Inject, Service } from 'typedi';
import config from '../../config';
import { Result } from '../core/logic/Result';
import { Floor } from '../domain/Floor/floor';
import IFloorDTO from '../dto/IFloorDTO';
import { FloorMap } from '../mappers/FloorMap';
import IBuildingService from '../services/IServices/IBuildingService';
import IFloorRepo from './IRepos/IFloorRepo';
import IFloorService from './IServices/IFloorService';

@Service()
export default class FloorService implements IFloorService {
  constructor(
    @Inject(config.repos.floor.name) private floorRepo: IFloorRepo,
    @Inject(config.services.building.name) private buildingService: IBuildingService,
  ) {}

  public async createFloor(floorDTO: IFloorDTO): Promise<Result<IFloorDTO>> {
    try {
      const floorOrError = FloorMap.toDomain(floorDTO);

      if (floorOrError.isFailure) {
        return Result.fail<IFloorDTO>(floorOrError.errorValue().toString());
      }

      const floorResult = floorOrError.getValue();

      const buildingResult = await this.buildingService.getBuildingByCode(floorResult.buildingCode.value);

      if (buildingResult.isFailure) {
        return Result.fail<IFloorDTO>('Building with the provided code does not exist.');
      }

      await this.floorRepo.save(floorResult);

      const floorDTOResult = FloorMap.toDTO(floorResult) as IFloorDTO;

      return Result.ok<IFloorDTO>(floorDTOResult);
    } catch (error) {
      throw error;
    }
  }
}
