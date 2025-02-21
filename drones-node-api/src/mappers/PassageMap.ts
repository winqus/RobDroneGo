import { UniqueEntityID } from '../core/domain/UniqueEntityID';
import { Mapper } from '../core/infra/Mapper';
import { Result } from '../core/logic/Result';
import { IPassagePersistence } from '../dataschema/IPassagePersistence';
import { Code as BuildingCode } from '../domain/Building/ValueObjects/code';
import { Passage } from '../domain/Passage/passage';
import IPassageDTO from '../dto/IPassageDTO';

export class PassageMap extends Mapper<Passage> {
  public static toDTO(passage: Passage): IPassageDTO {
    return {
      id: passage.id.toString(),
      buildingCode1: passage.buildingCode1.value,
      buildingCode2: passage.buildingCode2.value,
      floorNumber1: passage.floorNumber1,
      floorNumber2: passage.floorNumber2,
    } as IPassageDTO;
  }

  public static toDomain(raw: any): Result<Passage> {
    const buildingCode1 = BuildingCode.create(raw?.buildingCode1 as string);
    const buildingCode2 = BuildingCode.create(raw?.buildingCode2 as string);
    const floorNumber1 = raw?.floorNumber1 as number;
    const floorNumber2 = raw?.floorNumber2 as number;

    const combinedResults = Result.combine([buildingCode1, buildingCode2]);
    if (combinedResults.isFailure) {
      return Result.fail<Passage>(combinedResults.error);
    }

    const passageOrError = Passage.create(
      {
        buildingCode1: buildingCode1.getValue(),
        buildingCode2: buildingCode2.getValue(),
        floorNumber1: floorNumber1,
        floorNumber2: floorNumber2,
      },
      new UniqueEntityID(raw.id),
    );

    passageOrError.isFailure ? console.log(passageOrError.error) : '';

    return passageOrError;
  }

  public static toPersistence(passage: Passage): IPassagePersistence {
    return {
      id: passage.id.toString(),
      buildingCode1: passage.buildingCode1.value,
      buildingCode2: passage.buildingCode2.value,
      floorNumber1: passage.floorNumber1,
      floorNumber2: passage.floorNumber2,
    };
  }
}
