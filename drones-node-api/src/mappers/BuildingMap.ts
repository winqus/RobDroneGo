import { Mapper } from '../core/infra/Mapper';

import { Document, Model } from 'mongoose';

import { UniqueEntityID } from '../core/domain/UniqueEntityID';
import { IBuildingPersistence } from '../dataschema/IBuildingPersistence';
import { Building } from '../domain/Building/building';
import IBuildingDTO from '../dto/IBuildingDTO';

export class BuildingMap extends Mapper<Building> {
  public static toDTO(building: Building): IBuildingDTO {
    return {
      id: building.id.toString(),
      name: building.name.value,
      code: building.code.value,
      description: building.description.value,
      floorSizeLength: building.floorSize.value.length,
      floorSizeWidth: building.floorSize.value.width,
    } as IBuildingDTO;
  }

  public static toDomain(building: any | Model<IBuildingPersistence & Document>): Building {
    const buildingOrError = Building.create(building, new UniqueEntityID(building.id.toString()));

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
    };
  }
}
