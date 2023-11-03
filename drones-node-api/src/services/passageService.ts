import { Inject, Service } from 'typedi';
import config from '../../config';
import { Result } from '../core/logic/Result';
import IPassageDTO from '../dto/IPassageDTO';
import { PassageMap } from '../mappers/PassageMap';
import IFloorRepo from './IRepos/IFloorRepo';
import IPassageRepo from './IRepos/IPassageRepo';
import IPassageService from './IServices/IPassageService';

@Service()
export default class PassageService implements IPassageService {
  constructor(
    @Inject(config.repos.passage.name) private passageRepo: IPassageRepo,
    @Inject(config.repos.floor.name) private floorRepo: IFloorRepo,
  ) {}

  public async getAllPassages(): Promise<Result<IPassageDTO[]>> {
    try {
      const passages = await this.passageRepo.getAll();

      const passagesDTO = passages.map((passage) => PassageMap.toDTO(passage));

      return Result.ok<IPassageDTO[]>(passagesDTO);
    } catch (error) {
      return Result.fail<IPassageDTO[]>(error);
    }
  }

  public async getPassagesBetweenBuildings(
    buildingCode1: string,
    buildingCode2: string,
  ): Promise<Result<IPassageDTO[]>> {
    try {
      const passages = await this.passageRepo.findAllByBuildingCodes(buildingCode1, buildingCode2);

      const passagesDTO = passages.map((passage) => PassageMap.toDTO(passage));

      return Result.ok<IPassageDTO[]>(passagesDTO);
    } catch (error) {
      return Result.fail<IPassageDTO[]>(error);
    }
  }

  public async createPassage(passageDTO: IPassageDTO): Promise<Result<IPassageDTO>> {
    try {
      const passageOrError = PassageMap.toDomain(passageDTO);

      if (passageOrError.isFailure) {
        return Result.fail<IPassageDTO>(passageOrError.errorValue().toString());
      }
      const newPassage = passageOrError.getValue();

      if (passageDTO.buildingCode1 === passageDTO.buildingCode2) {
        return Result.fail<IPassageDTO>('Building codes must be different');
      }

      const existingPassage = await this.passageRepo.findByCodes(newPassage);
      const existingPassageSwapped = await this.passageRepo.findByCodes(
        PassageMap.toDomain({
          buildingCode1: passageDTO.buildingCode2,
          buildingCode2: passageDTO.buildingCode1,
          floorNumber1: passageDTO.floorNumber2,
          floorNumber2: passageDTO.floorNumber1,
        }).getValue(),
      );

      if (existingPassage !== null || existingPassageSwapped !== null) {
        return Result.fail<IPassageDTO>('Passage already exists');
      }

      const floor1 = await this.floorRepo.findByCode(passageDTO.buildingCode1, passageDTO.floorNumber1);
      const floor2 = await this.floorRepo.findByCode(passageDTO.buildingCode2, passageDTO.floorNumber2);

      if (floor1 === null) {
        return Result.fail<IPassageDTO>('Floor1 does not belong to building1 that might not exist');
      }

      if (floor2 === null) {
        return Result.fail<IPassageDTO>('Floor2 does not belong to building2 that might not exist');
      }

      await this.passageRepo.save(newPassage);

      const passageDTOResult = PassageMap.toDTO(newPassage) as IPassageDTO;

      return Result.ok<IPassageDTO>(passageDTOResult);
    } catch (error) {
      return Result.fail<IPassageDTO>(error);
    }
  }
}
