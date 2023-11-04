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

  public async updateFloor(updatedFloorDTO: IFloorDTO): Promise<Result<IFloorDTO>> {
    try {
      const floorOrError = FloorMap.toDomain(updatedFloorDTO);

      if (floorOrError.isFailure) {
        return Result.fail<IFloorDTO>(floorOrError.errorValue().toString());
      }

      const floorResult = floorOrError.getValue();

      const buildingResult = await this.buildingService.getBuildingByCode(floorResult.buildingCode.value);

      if (buildingResult.isFailure) {
        return Result.fail<IFloorDTO>('Building with the provided code does not exist.');
      }

      const floorResultExists = await this.floorRepo.exists(floorResult);
      if (!floorResultExists) {
        return Result.fail<IFloorDTO>('Floor does not exist.');
      }

      await this.floorRepo.save(floorResult);

      const floorDTOResult = FloorMap.toDTO(floorResult) as IFloorDTO;

      return Result.ok<IFloorDTO>(floorDTOResult);
    } catch (error) {
      throw error;
    }
  }

  async partialUpdateFloor(updatedFloorDTO: Partial<IFloorDTO>): Promise<Result<IFloorDTO>> {
    try {
      const existingFloor = await this.floorRepo.findById(updatedFloorDTO.id);
      if (!existingFloor) {
        return Result.fail<IFloorDTO>('Floor does not exist.');
      }

      const updatedFloorData = {
        id: existingFloor.id.toString(),
        floorNumber: existingFloor.floorNumber,
        buildingCode: existingFloor.buildingCode.value,
        description: updatedFloorDTO.description || existingFloor.description.value,
        servedByElevator: existingFloor.servedByElevator,
      };

      const floorOrError = FloorMap.toDomain(updatedFloorData);

      if (floorOrError.isFailure) {
        return Result.fail<IFloorDTO>(floorOrError.errorValue().toString());
      }

      await this.floorRepo.save(floorOrError.getValue());

      const floorDTOResult = FloorMap.toDTO(floorOrError.getValue()) as IFloorDTO;

      return Result.ok<IFloorDTO>(floorDTOResult);
    } catch (error) {
      throw error;
    }
  }

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

  public async getAllFloors(): Promise<Result<IFloorDTO[]>> {
    try {
      const floors = await this.floorRepo.findAllFloors();

      const floorDTOs: IFloorDTO[] = floors.map((floor: Floor) => {
        return FloorMap.toDTO(floor);
      });

      return Result.ok<IFloorDTO[]>(floorDTOs);
    } catch (error) {
      return Result.fail<IFloorDTO[]>(error);
    }
  }

  public async getFloorsByBuildingCode(buildingCode: string): Promise<Result<IFloorDTO[]>> {
    try {
      const floors = await this.floorRepo.findByBuildingCode(buildingCode);

      const floorDTOs: IFloorDTO[] = floors.map((floor: Floor) => {
        return FloorMap.toDTO(floor);
      });

      return Result.ok<IFloorDTO[]>(floorDTOs);
    } catch (error) {
      return Result.fail<IFloorDTO[]>(error);
    }
  }

  public async getFloorsServedByElevator(buildingCode: string): Promise<Result<IFloorDTO[]>> {
    try {
      const floors = await this.floorRepo.findByBuildingCode(buildingCode);

      const floorDTOs: IFloorDTO[] = floors
        .filter((floor: Floor) => floor.servedByElevator)
        .map((floor: Floor) => {
          return FloorMap.toDTO(floor);
        });

      return Result.ok<IFloorDTO[]>(floorDTOs);
    } catch (error) {
      return Result.fail<IFloorDTO[]>(error);
    }
  }
}
