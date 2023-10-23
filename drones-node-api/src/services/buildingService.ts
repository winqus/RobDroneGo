import { Inject, Service } from 'typedi';
import config from '../../config';
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

      await this.buildingRepo.save(buildingResult);

      const buildingDTOResult = BuildingMap.toDTO(buildingResult) as IBuildingDTO;

      return Result.ok<IBuildingDTO>(buildingDTOResult);
    } catch (error) {
      throw error;
    }
  }
}
