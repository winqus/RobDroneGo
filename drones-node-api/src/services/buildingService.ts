import { Inject, Service } from 'typedi';
import config from '../../config';
import { UniqueEntityID } from '../core/domain/UniqueEntityID';
import { Result } from '../core/logic/Result';
import { Code } from '../domain/Building/ValueObjects/code';
import { Description } from '../domain/Building/ValueObjects/description';
import { FloorSize } from '../domain/Building/ValueObjects/floorSize';
import { Name } from '../domain/Building/ValueObjects/name';
import { Building } from '../domain/Building/building';
import IBuildingDTO from '../dto/IBuildingDTO';
import { BuildingMap } from '../mappers/BuildingMap';
import IBuildingRepo from './IRepos/IBuildingRepo';
import IBuildingService from './IServices/IBuildingService';

@Service()
export default class BuildingService implements IBuildingService {
  constructor(@Inject(config.repos.building.name) private buildingRepo: IBuildingRepo) {}

  public async createBuilding(buildingDTO: IBuildingDTO): Promise<Result<IBuildingDTO>> {
    try {
      const nameResult = await Name.create(buildingDTO.name);
      const codeResult = await Code.create(buildingDTO.code);
      const descriptionResult = await Description.create(buildingDTO.description);
      const floorSizeResult = await FloorSize.create(buildingDTO.floorSizeLength, buildingDTO.floorSizeWidth);

      const combinedResult = Result.combine([nameResult, codeResult, descriptionResult, floorSizeResult]);
      if (combinedResult.isFailure) {
        return Result.fail<IBuildingDTO>(combinedResult.error);
      }

      const buildingOrError = Building.create({
        name: nameResult.getValue(),
        code: codeResult.getValue(),
        description: descriptionResult.getValue(),
        floorSize: floorSizeResult.getValue(),
      });

      if (buildingOrError.isFailure) {
        return Result.fail<IBuildingDTO>(buildingOrError.errorValue().toString());
      }

      const buildingResult = buildingOrError.getValue();

      try {
        await this.buildingRepo.save(buildingResult);
      } catch (error) {
        return Result.fail<IBuildingDTO>(error);
      }

      const buildingDTOResult = BuildingMap.toDTO(buildingResult) as IBuildingDTO;

      return Result.ok<IBuildingDTO>(buildingDTOResult);
    } catch (error) {
      throw error;
    }
  }

  public async updateBuilding(buildingDTO: IBuildingDTO): Promise<Result<IBuildingDTO>> {
    try {
      const buildingId = buildingDTO.id;
      const buildingToUpdate = await this.buildingRepo.findById(buildingId);

      if (buildingToUpdate === null) {
        return Result.fail<IBuildingDTO>('Building not found');
      }

      const nameResult = buildingDTO.name ? await Name.create(buildingDTO.name) : Result.ok(buildingToUpdate.name);
      const descriptionResult = buildingDTO.description
        ? await Description.create(buildingDTO.description)
        : Result.ok(buildingToUpdate.description);
      const floorSizeResult = await FloorSize.create(
        buildingDTO.floorSizeLength ?? buildingToUpdate.floorSize.value.length,
        buildingDTO.floorSizeWidth ?? buildingToUpdate.floorSize.value.width,
      );

      const combinedResult = Result.combine([nameResult, descriptionResult, floorSizeResult]);
      if (combinedResult.isFailure) {
        return Result.fail<IBuildingDTO>(combinedResult.error);
      }

      buildingToUpdate.name = nameResult.getValue();
      buildingToUpdate.description = descriptionResult.getValue();
      buildingToUpdate.floorSize = floorSizeResult.getValue();

      try {
        await this.buildingRepo.save(buildingToUpdate);
      } catch (error) {
        return Result.fail<IBuildingDTO>(error);
      }

      const buildingDTOResult = BuildingMap.toDTO(buildingToUpdate) as IBuildingDTO;

      return Result.ok<IBuildingDTO>(buildingDTOResult);
    } catch (error) {
      throw error;
    }
  }
}
