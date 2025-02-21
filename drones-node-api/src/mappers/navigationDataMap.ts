import { Mapper } from '../core/infra/Mapper';
import { Result } from '../core/logic/Result';
import { INavigationDataPersistence } from '../dataschema/INavigationDataPersistence';
import { NavigationData } from '../domain/TaskRequest/navigationData';

export class NavigationDataMap extends Mapper<NavigationData> {
  public static toDTO(navigationData: NavigationData): INavigationDataDTO {
    const floorsPaths = navigationData.floorsPaths.map((floorPath) => {
      return {
        fromBuilding: floorPath.fromBuilding,
        fromFloorNumber: floorPath.fromFloorNumber,
        toBuilding: floorPath.toBuilding,
        toFloorNumber: floorPath.toFloorNumber,
        type: floorPath.type,
      };
    });
    const mapPaths = navigationData.mapPaths.map((mapPath) => {
      return {
        buildingCode: mapPath.buildingCode,
        cost: mapPath.cost,
        floorNumber: mapPath.floorNumber,
        path: mapPath.path.map((pathPoint) => {
          return {
            col: pathPoint.col,
            row: pathPoint.row,
          };
        }),
      };
    });

    return {
      floorsPaths: floorsPaths,
      mapPathCount: navigationData.mapPathCount,
      mapPaths: mapPaths,
    } as INavigationDataDTO;
  }

  public static toDomain(raw: any): Result<NavigationData> {
    const floorsPaths = raw?.floorsPaths as FloorPath[];
    const mapPathCount = raw?.mapPathCount as number;
    const mapPaths = raw?.mapPaths as MapPath[];

    const navigationDataOrError = NavigationData.create({
      floorsPaths: floorsPaths,
      mapPathCount: mapPathCount,
      mapPaths: mapPaths,
    });

    navigationDataOrError.isFailure ? console.log(navigationDataOrError.error) : '';

    return navigationDataOrError;
  }

  public static toPersistence(navigationData: NavigationData): INavigationDataPersistence {
    const floorsPaths = navigationData.floorsPaths.map((floorPath) => {
      return {
        fromBuilding: floorPath.fromBuilding,
        fromFloorNumber: floorPath.fromFloorNumber,
        toBuilding: floorPath.toBuilding,
        toFloorNumber: floorPath.toFloorNumber,
        type: floorPath.type,
      };
    });
    const mapPaths = navigationData.mapPaths.map((mapPath) => {
      return {
        buildingCode: mapPath.buildingCode,
        cost: mapPath.cost,
        floorNumber: mapPath.floorNumber,
        path: mapPath.path.map((pathPoint) => {
          return {
            col: pathPoint.col,
            row: pathPoint.row,
          };
        }),
      };
    });

    return {
      floorsPaths: floorsPaths,
      mapPathCount: navigationData.mapPathCount,
      mapPaths: mapPaths,
    } as INavigationDataPersistence;
  }
}
