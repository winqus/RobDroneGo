import { ValueObject } from '../../core/domain/ValueObject';
import { Guard } from '../../core/logic/Guard';
import { Result } from '../../core/logic/Result';

interface NavigationDataProps {
  floorsPaths: FloorPath[];
  mapPathCount: number;
  mapPaths: MapPath[];
}

interface FloorPath {
  fromBuilding: string;
  fromFloorNumber: string;
  toBuilding: string;
  toFloorNumber: string;
  type: string;
}

interface MapPath {
  buildingCode: string;
  cost: number;
  floorNumber: number;
  path: PathPoint[];
}

interface PathPoint {
  col: number;
  row: number;
}

export class NavigationData extends ValueObject<NavigationDataProps> {
  get floorsPaths(): FloorPath[] {
    return this.props.floorsPaths;
  }

  get mapPathCount(): number {
    return this.props.mapPathCount;
  }

  get mapPaths(): MapPath[] {
    return this.props.mapPaths;
  }

  set FloorsPaths(floorsPaths: FloorPath[]) {
    this.props.floorsPaths = floorsPaths;
  }

  set MapPathCount(mapPathCount: number) {
    this.props.mapPathCount = mapPathCount;
  }

  set MapPaths(mapPaths: MapPath[]) {
    this.props.mapPaths = mapPaths;
  }

  private constructor(props: NavigationDataProps) {
    super(props);
  }

  public static create(props: NavigationDataProps): Result<NavigationData> {
    const guardedProps = [
      { argument: props.floorsPaths, argumentName: 'floorsPaths' },
      { argument: props.mapPathCount, argumentName: 'mapPathCount' },
      { argument: props.mapPaths, argumentName: 'mapPaths' },
    ];

    const navigationGuardResult = Guard.againstNullOrUndefinedBulk(guardedProps);

    if (!navigationGuardResult.succeeded) {
      return Result.fail<NavigationData>(navigationGuardResult.message);
    } else {
      return Result.ok<NavigationData>(new NavigationData(props));
    }
  }
}
