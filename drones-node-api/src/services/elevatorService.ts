import { Inject, Service } from 'typedi';
import config from '../../config';
import { UniqueEntityID } from '../core/domain/UniqueEntityID';
import { Result } from '../core/logic/Result';
import { Description } from '../domain/Building/Entities/ValueObjects/description';
import { IDNumber } from '../domain/Building/Entities/ValueObjects/idNumber';
import { MakeModel } from '../domain/Building/Entities/ValueObjects/makeModel';
import { SerialNumber } from '../domain/Building/Entities/ValueObjects/serialNumber';
import { Elevator } from '../domain/Building/Entities/elevator';
import IBuildingDTO from '../dto/IBuildingDTO';
import IElevatorDTO from '../dto/IElevatorDTO';
import { BuildingMap } from '../mappers/BuildingMap';
import IBuildingRepo from './IRepos/IBuildingRepo';
import IFloorRepo from './IRepos/IFloorRepo';
import IElevatorService from './IServices/IElevadorService';

@Service()
export default class ElevatorService implements IElevatorService {
  constructor(
    @Inject(config.repos.building.name) private buildingRepo: IBuildingRepo,
    @Inject(config.repos.floor.name) private floorRepo: IFloorRepo,
  ) {}

  public async createElevator(
    elevatorDTO: IElevatorDTO,
    buildingCode: string,
    floors: number[],
  ): Promise<Result<IBuildingDTO>> {
    try {
      const building = await this.buildingRepo.findByCode(buildingCode);
      if (building === null) {
        return Result.fail<IBuildingDTO>('Building not found');
      }
      if (building.elevator !== null && building.elevator !== undefined) {
        return Result.fail<IBuildingDTO>('Building already has an elevator');
      }

      const numberResult = await IDNumber.create(elevatorDTO.number);
      const makeModelResult = await MakeModel.create(elevatorDTO.make, elevatorDTO.model);
      const serialNumberResult = await SerialNumber.create(elevatorDTO.serialNumber);
      const descriptionResult = await Description.create(elevatorDTO.description);

      const combinedResult = Result.combine([numberResult, makeModelResult, serialNumberResult, descriptionResult]);
      if (combinedResult.isFailure) {
        return Result.fail<IBuildingDTO>(combinedResult.error);
      }

      const elevatorOrError = Elevator.create({
        number: numberResult.getValue(),
        makeModel: makeModelResult.getValue(),
        serialNumber: serialNumberResult.getValue(),
        description: descriptionResult.getValue(),
      });

      if (elevatorOrError.isFailure) {
        return Result.fail<IBuildingDTO>(elevatorOrError.errorValue().toString());
      }

      const elevatorResult = elevatorOrError.getValue();

      building.elevator = elevatorResult;

      const floorSResult = await this.floorRepo.findByCodes(buildingCode, floors);

      for (const floor of floorSResult) {
        floor.servedByElevator = true;
      }

      try {
        await this.buildingRepo.save(building);
        for (const floor of floorSResult) {
          await this.floorRepo.save(floor);
        }
      } catch (error) {
        return Result.fail<IBuildingDTO>(error);
      }

      const buildingDTOResult = BuildingMap.toDTO(building) as IBuildingDTO;

      return Result.ok<IBuildingDTO>(buildingDTOResult);
    } catch (error) {
      return Result.fail<IBuildingDTO>(error);
    }
  }
}
