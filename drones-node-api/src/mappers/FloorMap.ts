import { Document } from 'mongodb';
import { UniqueEntityID } from '../core/domain/UniqueEntityID';
import { Mapper } from '../core/infra/Mapper';
import { Result } from '../core/logic/Result';
import { Code as BuildingCode } from '../domain/Building/ValueObjects/code';
import { Description } from '../domain/Building/ValueObjects/description';
import { ElevatorExit } from '../domain/Floor/ValueObject/elevatorExit';
import { Map } from '../domain/Floor/ValueObject/map';
import { PassageExit } from '../domain/Floor/ValueObject/passageExit';
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
      map: floor.map
        ? {
            size: { width: floor.map.width, height: floor.map.height },
            map: floor.map.map,
            exitLocations: floor.map.exitLocations
              ? {
                  passages: floor.map.exitLocations.passages.map((passage) => {
                    return {
                      cellPosition: passage.cellPosition,
                      destination: {
                        buildingCode: passage.destination.buildingCode,
                        floorNumber: passage.destination.floorNumber,
                      },
                    };
                  }),
                  elevators: floor.map.exitLocations.elevators.map((elevator) => {
                    return {
                      cellPosition: elevator.cellPosition,
                    };
                  }),
                }
              : null,
          }
        : null,
    } as IFloorDTO;
  }

  public static toDomain(raw: any): Result<Floor> {
    const floorNumber = raw?.floorNumber as number;
    const description = Description.create(raw?.description as string);

    if (raw.servedByElevator === undefined || raw.servedByElevator === null) {
      raw.servedByElevator = false;
    }
    const servedByElevator = raw?.servedByElevator as boolean;
    const buildingCode = BuildingCode.create(raw?.buildingCode as string);

    let passageExit: PassageExit[] | null = null;
    let elevatorExit: ElevatorExit[] | null = null;

    if (raw.map && raw.map?.exitLocations) {
      passageExit = raw.map?.exitLocations?.passages.map((passage: any) => {
        return PassageExit.create([passage.cellPosition[0], passage.cellPosition[1]], {
          buildingCode: passage.destination.buildingCode as string,
          floorNumber: passage.destination.floorNumber as number,
        }).getValue();
      });

      elevatorExit = raw.map?.exitLocations?.elevators.map((elevator: any) => {
        return ElevatorExit.create(elevator.cellPosition).getValue();
      });
    }

    const combinedResults = Result.combine([description, buildingCode]);
    if (combinedResults.isFailure) {
      return Result.fail<Floor>(combinedResults.error);
    }
    let map: Map | null = null;
    if (raw.map) {
      if (passageExit === null && elevatorExit === null) {
        map = Map.create(
          raw.map.size.width as number,
          raw.map.size.height as number,
          raw.map?.map as number[][],
        ).getValue();
      } else {
        map = Map.create(raw?.map.size.width as number, raw?.map.size.height as number, raw?.map?.map as number[][], {
          passages: passageExit,
          elevators: elevatorExit,
        }).getValue();
      }
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
      map: floor.map
        ? {
            size: {
              width: floor.map.width,
              height: floor.map.height,
            },
            map: floor.map.map,
            exitLocations: {
              passages: floor.map.exitLocations.passages.map((passage) => {
                return {
                  cellPosition: passage.cellPosition,
                  destination: {
                    buildingCode: passage.destination.buildingCode,
                    floorNumber: passage.destination.floorNumber,
                  },
                };
              }),
              elevators: floor.map.exitLocations.elevators.map((elevator) => {
                return {
                  cellPosition: elevator.cellPosition,
                };
              }),
            },
          }
        : null,
    };
  }
}
