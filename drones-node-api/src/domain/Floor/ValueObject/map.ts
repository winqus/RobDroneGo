import { ValueObject } from '../../../core/domain/ValueObject';

import { Guard } from '../../../core/logic/Guard';
import { Result } from '../../../core/logic/Result';

interface MapProps {
  size: { width: number; height: number };
  map: number[][];
}

export class Map extends ValueObject<MapProps> {
  get width(): number {
    return this.props.size.width;
  }

  get height(): number {
    return this.props.size.height;
  }

  get map(): number[][] {
    return this.props.map;
  }

  private constructor(props: MapProps) {
    super(props);
  }

  public static create(width: number, height: number, map: number[][]): Result<Map> {
    const guardResult = Guard.combine([
      Guard.isInteger(width, 'width'),
      Guard.isInteger(height, 'height'),
      Guard.isTrue(width > 0, 'width must be greater than 0'),
      Guard.isTrue(height > 0, 'height must be greater than 0'),
      Guard.isTrue(map.length === height, 'map height must match height'),
      Guard.isTrue(map[0].length === width, 'map width must match width'),
    ]);

    if (!guardResult.succeeded) {
      return Result.fail<Map>(guardResult.message);
    } else {
      return Result.ok<Map>(new Map({ size: { width, height }, map }));
    }
  }
}
