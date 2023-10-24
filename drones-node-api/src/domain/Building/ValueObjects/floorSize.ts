import { ValueObject } from '../../../core/domain/ValueObject';

import { Guard } from '../../../core/logic/Guard';
import { Result } from '../../../core/logic/Result';

interface FloorSizeProps {
  length: number;
  width: number;
}

export class FloorSize extends ValueObject<FloorSizeProps> {
  get value(): FloorSizeProps {
    return {
      length: this.props.length,
      width: this.props.width,
    };
  }

  private constructor(props: FloorSizeProps) {
    super(props);
  }

  public static create(length: number, width: number): Result<FloorSize> {
    const guardResult = Guard.combine([
      Guard.againstNullOrUndefined(length, 'length'),
      Guard.againstNullOrUndefined(width, 'width'),
      Guard.isTrue(length > 0, 'length must be greater than 0'),
      Guard.isTrue(width > 0, 'width must be greater than 0'),
    ]);

    if (!guardResult.succeeded) {
      return Result.fail<FloorSize>(guardResult.message);
    } else {
      return Result.ok<FloorSize>(new FloorSize({ length, width }));
    }
  }
}
