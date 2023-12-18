import { UniqueEntityID } from '../core/domain/UniqueEntityID';
import { Mapper } from '../core/infra/Mapper';
import { Result } from '../core/logic/Result';
import { ISurveillanceTaskPersistence } from '../dataschema/ISurveillanceTaskPersistence';
import { Code as BuildingCode } from '../domain/Building/ValueObjects/code';
import { SurveillanceTask } from '../domain/SurveillanceTask/surveillanceTask';
import { ISurveillanceTaskDTO } from '../dto/ISurveillanceTaskDTO';

export class SurveillanceTaskMap extends Mapper<SurveillanceTask> {
  public static toDTO(surveillanceTask: SurveillanceTask): ISurveillanceTaskDTO {
    return {
      id: surveillanceTask.id.toString(),
      buildingCode: surveillanceTask.buildingCode.value,
      floorNumber: surveillanceTask.floorNumber,
      contactNumber: surveillanceTask.contactNumber,
    } as ISurveillanceTaskDTO;
  }

  public static toDomain(raw: any): Result<SurveillanceTask> {
    const buildingCode = BuildingCode.create(raw?.buildingCode as string);
    const floorNumber = raw?.floorNumber as number[];
    const contactNumber = raw?.contactNumber as number;

    const combinedResults = Result.combine([buildingCode]);
    if (combinedResults.isFailure) {
      return Result.fail<SurveillanceTask>(combinedResults.error);
    }

    const surveillanceTaskOrError = SurveillanceTask.create(
      {
        buildingCode: buildingCode.getValue(),
        floorNumber: floorNumber,
        contactNumber: contactNumber,
      },
      new UniqueEntityID(raw.id),
    );

    surveillanceTaskOrError.isFailure ? console.log(surveillanceTaskOrError.error) : '';

    return surveillanceTaskOrError;
  }

  public static toPersistence(surveillanceTask: SurveillanceTask): ISurveillanceTaskPersistence {
    return {
      id: surveillanceTask.id.toString(),
      buildingCode: surveillanceTask.buildingCode.value,
      floorNumber: surveillanceTask.floorNumber,
      contactNumber: surveillanceTask.contactNumber,
    } as ISurveillanceTaskPersistence;
  }
}
