import { ValueObject } from '../../../core/domain/ValueObject';

import { Result } from '../../../core/logic/Result';
import { Guard } from '../../../core/logic/Guard';

interface FloorSizeProps {
  width: number;
  height: number;
}

export class FloorSize extends ValueObject<FloorSizeProps> {
  get value(): FloorSizeProps {
    return {
      width: this.props.width,
      height: this.props.height,
    };
  }

  private constructor(props: FloorSizeProps) {
    super(props);
  }

  public static create(width: number, height: number): Result<FloorSize> {
    const guardResult = Guard.combine([
      Guard.againstNullOrUndefined(width, 'width'),
      Guard.againstNullOrUndefined(height, 'height'),
      Guard.isTrue(width > 0, 'width must be greater than 0'),
      Guard.isTrue(height > 0, 'height must be greater than 0'),
    ]);

    if (!guardResult.succeeded) {
      return Result.fail<FloorSize>(guardResult.message);
    } else {
      return Result.ok<FloorSize>(new FloorSize({ width, height }));
    }
  }
}
