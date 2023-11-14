import { Inject, Service } from 'typedi';
import config from '../../config';
import { Result } from '../core/logic/Result';
import { Code } from '../domain/Building/ValueObjects/code';
import IFloorDTO from '../dto/IFloorDTO';
import IPassageDTO from '../dto/IPassageDTO';
import { FloorMap } from '../mappers/FloorMap';
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

  public async listFloorsWithPassagesToDifferentBuilding(buildingCode: string): Promise<Result<IFloorDTO[]>> {
    try {
      const passages = await this.passageRepo.getPassagesToDiferentBuildings(buildingCode);

      let listFloorsDTO = await Promise.all(
        passages.map(async (passage) => {
          let floor;
          if (passage.buildingCode1.value === buildingCode) {
            floor = await this.floorRepo.findByCode(passage.buildingCode1.value, passage.floorNumber1);
          } else {
            floor = await this.floorRepo.findByCode(passage.buildingCode2.value, passage.floorNumber2);
          }

          return FloorMap.toDTO(floor);
        }),
      );

      listFloorsDTO = listFloorsDTO.filter(
        (value, index, array) => index == array.findIndex((item) => item.id == value.id),
      );

      return Result.ok<IFloorDTO[]>(listFloorsDTO);
    } catch (error) {
      return Result.fail<IFloorDTO[]>(error);
    }
  }

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

      const passage = await this.passageRepo.findByCodes(newPassage);
      const existingPassageSwapped = await this.passageRepo.findByCodes(
        PassageMap.toDomain({
          buildingCode1: passageDTO.buildingCode2,
          buildingCode2: passageDTO.buildingCode1,
          floorNumber1: passageDTO.floorNumber2,
          floorNumber2: passageDTO.floorNumber1,
        }).getValue(),
      );

      if (passage !== null || existingPassageSwapped !== null) {
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

  public async updatePassage(oldPassageDTO: IPassageDTO, newPassageDTO: IPassageDTO): Promise<Result<IPassageDTO>> {
    try {
      let floor = await this.floorRepo.findByCode(newPassageDTO.buildingCode1, newPassageDTO.floorNumber1);
      if (floor === null) {
        return Result.fail<IPassageDTO>('Floor1 does not belong to building1 that might not exist');
      }
      floor = await this.floorRepo.findByCode(newPassageDTO.buildingCode2, newPassageDTO.floorNumber2);
      if (floor === null) {
        return Result.fail<IPassageDTO>('Floor2 does not belong to building2 that might not exist');
      }

      const oldPassageOrError = PassageMap.toDomain(oldPassageDTO);

      if (oldPassageOrError.isFailure) {
        return Result.fail<IPassageDTO>(oldPassageOrError.errorValue().toString());
      }

      const oldPassage = oldPassageOrError.getValue();

      const passage = await this.passageRepo.findByCodes(oldPassage);

      if (passage === null) {
        return Result.fail<IPassageDTO>('Passage does not exist');
      }

      passage.buildingCode1 = Code.create(newPassageDTO.buildingCode1).getValue();
      passage.buildingCode2 = Code.create(newPassageDTO.buildingCode2).getValue();
      passage.floorNumber1 = newPassageDTO.floorNumber1;
      passage.floorNumber2 = newPassageDTO.floorNumber2;

      await this.passageRepo.save(passage);

      const passageDTOResult = PassageMap.toDTO(passage) as IPassageDTO;

      return Result.ok<IPassageDTO>(passageDTOResult);
    } catch (error) {
      return Result.fail<IPassageDTO>(error);
    }
  }
}
