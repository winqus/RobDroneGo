import { UniqueEntityID } from '../core/domain/UniqueEntityID';
import { Mapper } from '../core/infra/Mapper';
import { Result } from '../core/logic/Result';
import { Code as BuildingCode } from '../domain/Building/ValueObjects/code';
import { Description } from '../domain/Building/ValueObjects/description';
import { Map } from '../domain/Floor/ValueObject/map';
import { Floor } from '../domain/Floor/floor';
import IFloorDTO from '../dto/IFloorDTO';

export class FloorMap extends Mapper<Floor> {
  public static toDTO(floor: Floor): IFloorDTO {
    return {
      id: floor.id.toString(),
      floorNumber: floor.floorNumber,
      description: floor.description.value,
      servedByElevator: floor.servedByElevator,
      buildingCode: floor.buildingCode.value,
      map: floor.map ? { size: { width: floor.map.width, height: floor.map.height }, map: floor.map.map } : null,
    } as IFloorDTO;
  }

  public static toDomain(raw: any): Result<Floor> {
    const floorNumber = raw?.floorNumber as number;
    const description = Description.create(raw?.description as string);
    const servedByElevator = raw?.servedByElevator as boolean;
    const buildingCode = BuildingCode.create(raw?.buildingCode as string);

    const combinedResults = Result.combine([description, buildingCode]);
    if (combinedResults.isFailure) {
      return Result.fail<Floor>(combinedResults.error);
    }
    let map: Map | null = null;
    if (
      raw?.map !== null &&
      raw?.map !== undefined &&
      raw?.map.map !== null &&
      raw?.map.map !== undefined &&
      raw?.map.map.length > 0
    ) {
      map = Map.create(raw?.map?.width as number, raw?.map?.height as number, raw?.map?.map as number[][]).getValue();
    }

    const floorOrError = Floor.create(
      {
        floorNumber: floorNumber,
        description: description.getValue(),
        servedByElevator,
        buildingCode: buildingCode.getValue(),
        map: map,
      },
      new UniqueEntityID(raw.id),
    );

    floorOrError.isFailure ? console.log(floorOrError.error) : '';

    return floorOrError;
  }

  public static toPersistence(floor: Floor): any {
    return {
      id: floor.id.toString(),
      floorNumber: floor.floorNumber,
      description: floor.description.value,
      servedByElevator: floor.servedByElevator,
      buildingCode: floor.buildingCode.value,
      map: floor.map ? { size: { width: floor.map.width, height: floor.map.height }, map: floor.map.map } : null,
    };
  }
}
