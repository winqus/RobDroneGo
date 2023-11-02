import { UniqueEntityID } from '../core/domain/UniqueEntityID';
import { Mapper } from '../core/infra/Mapper';
import { Code } from '../domain/Building/ValueObjects/code';
import { Description } from '../domain/Building/ValueObjects/description';
import { FloorSize } from '../domain/Building/ValueObjects/floorSize';
import { Name } from '../domain/Building/ValueObjects/name';
import { Building } from '../domain/Building/building';
import IBuildingDTO from '../dto/IBuildingDTO';
import { ElevatorMap } from './ElevatorMap';

export class BuildingMap extends Mapper<Building> {
  public static toDTO(building: Building): IBuildingDTO {
    return {
      id: building.id.toString(),
      name: building.name.value,
      code: building.code.value,
      description: building.description.value,
      floorSizeLength: building.floorSize.value.length,
      floorSizeWidth: building.floorSize.value.width,
      elevator: building.elevator ? ElevatorMap.toDTO(building.elevator) : null,
    } as IBuildingDTO;
  }

  public static toDomain(raw: any): Building {
    const name = Name.create(raw.name).getValue();
    const code = Code.create(raw.code).getValue();
    const description = Description.create(raw.description).getValue();
    const floorSize = FloorSize.create(raw.floorSizeLength, raw.floorSizeWidth).getValue();

    // Handle the elevator relationship
    const elevator = raw.elevator ? ElevatorMap.toDomain(raw.elevator) : null;

    const buildingOrError = Building.create(
      {
        name,
        code,
        description,
        floorSize,
        elevator, // Assign the elevator to the Building
      },
      new UniqueEntityID(raw.id),
    );

    buildingOrError.isFailure ? console.log(buildingOrError.error) : '';

    return buildingOrError.isSuccess ? buildingOrError.getValue() : null;
  }

  public static toPersistence(building: Building): any {
    return {
      id: building.id.toString(),
      name: building.name.value,
      code: building.code.value,
      description: building.description.value,
      floorSizeLength: building.floorSize.value.length,
      floorSizeWidth: building.floorSize.value.width,
      elevator: building.elevator ? ElevatorMap.toPersistence(building.elevator) : null,
    };
  }
}
