import { Inject, Service } from 'typedi';
import config from '../../config';
import { Result } from '../core/logic/Result';
import { ElevatorExit } from '../domain/Floor/ValueObject/elevatorExit';
import { Map } from '../domain/Floor/ValueObject/map';
import { PassageExit } from '../domain/Floor/ValueObject/passageExit';
import { Floor } from '../domain/Floor/floor';
import IFloorDTO from '../dto/IFloorDTO';
import IMapDTO from '../dto/IMapDTO';
import { FloorMap } from '../mappers/FloorMap';
import IBuildingService from '../services/IServices/IBuildingService';
import IBuildingRepo from './IRepos/IBuildingRepo';
import IFloorRepo from './IRepos/IFloorRepo';
import IFloorService from './IServices/IFloorService';

@Service()
export default class FloorService implements IFloorService {
  constructor(
    @Inject(config.repos.floor.name) private floorRepo: IFloorRepo,
    @Inject(config.services.building.name) private buildingService: IBuildingService,
    @Inject(config.repos.building.name) private buildingRepo: IBuildingRepo,
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
      const building = await this.buildingRepo.findByCode(buildingCode);

      if (building === null) {
        return Result.fail<IFloorDTO[]>('Building with the provided code does not exist.');
      }
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

  public async loadMap(
    buildingCode: string,
    floorNumber: number,
    map: {
      size: { width: number; height: number };
      map: number[][];
      exitLocations?: {
        passages: { cellPosition: [number, number]; destination: { buildingCode: string; floorNumber: number } }[];
        elevators: { cellPosition: [number, number] }[];
      };
    },
  ): Promise<Result<IFloorDTO>> {
    try {
      const building = await this.buildingRepo.findByCode(buildingCode);

      if (building === null) {
        return Result.fail<IFloorDTO>('Building with the provided code does not exist.');
      }
      if (building.floorSize.value.width < map.size.width || building.floorSize.value.length < map.size.height) {
        return Result.fail<IFloorDTO>('Map size is bigger than the maximum floor size of the building.');
      }

      const floor = await this.floorRepo.findByCode(buildingCode, floorNumber);
      let mapObject: Result<Map>;
      if (map.exitLocations) {
        const passagesExits = map.exitLocations.passages.map((passage) => {
          return PassageExit.create(passage.cellPosition, {
            buildingCode: passage.destination.buildingCode,
            floorNumber: passage.destination.floorNumber,
          }).getValue();
        });

        const elevatorsExits = map.exitLocations.elevators.map((elevator) => {
          return ElevatorExit.create(elevator.cellPosition).getValue();
        });

        mapObject = Map.create(map.size.width, map.size.height, map.map, {
          passages: passagesExits,
          elevators: elevatorsExits,
        });

        if (mapObject.isFailure) {
          return Result.fail<IFloorDTO>(mapObject.errorValue().toString());
        }
      } else {
        mapObject = Map.create(map.size.width, map.size.height, map.map);

        if (mapObject.isFailure) {
          return Result.fail<IFloorDTO>(mapObject.errorValue().toString());
        }
      }

      floor.map = mapObject.getValue();

      this.floorRepo.save(floor);

      const floorDTOResult = FloorMap.toDTO(floor) as IFloorDTO;

      return Result.ok<IFloorDTO>(floorDTOResult);
    } catch (error) {
      throw error;
    }
  }

  public async getMap(buildingCode: string, floorNumber: number): Promise<Result<IMapDTO>> {
    try {
      const building = await this.buildingRepo.findByCode(buildingCode);

      if (building === null) {
        return Result.fail<IMapDTO>('Building with the provided code does not exist.');
      }

      const floor = await this.floorRepo.findByCode(buildingCode, floorNumber);

      if (floor === null) {
        return Result.fail<IMapDTO>('Floor with the provided number does not exist.');
      }

      if (floor.map === null) {
        return Result.fail<IMapDTO>('Floor map is not loaded.');
      }

      const mapDTO: IMapDTO = FloorMap.toDTO(floor).map as IMapDTO;

      return Result.ok<IMapDTO>(mapDTO);
    } catch (error) {
      throw error;
    }
  }
}
