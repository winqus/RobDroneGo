import { ValueObject } from '../../core/domain/ValueObject';

import { Guard } from '../../core/logic/Guard';
import { Result } from '../../core/logic/Result';

interface PositionOnMapProps {
  x: number;
  y: number;
}

export class PositionOnMap extends ValueObject<PositionOnMapProps> {
  get value(): PositionOnMapProps {
    return {
      x: this.props.x,
      y: this.props.y,
    };
  }

  private constructor(props: PositionOnMapProps) {
    super(props);
  }

  public static create(x: number, y: number): Result<PositionOnMap> {
    const guardResult = Guard.combine([
      Guard.againstNullOrUndefined(x, 'x'),
      Guard.againstNullOrUndefined(y, 'y'),
      Guard.isTrue(x >= 0, 'x must be no less than 0'),
      Guard.isTrue(y >= 0, 'y must be no less than 0'),
    ]);

    if (!guardResult.succeeded) {
      return Result.fail<PositionOnMap>(guardResult.message);
    } else {
      return Result.ok<PositionOnMap>(new PositionOnMap({ x, y }));
    }
  }
}
