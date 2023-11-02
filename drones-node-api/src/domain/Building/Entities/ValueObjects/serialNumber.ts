import { ValueObject } from '../../../../core/domain/ValueObject';

import { Guard } from '../../../../core/logic/Guard';
import { Result } from '../../../../core/logic/Result';

interface SerialNumberProps {
  value: string;
}

export class SerialNumber extends ValueObject<SerialNumberProps> {
  get value(): string {
    return this.props.value;
  }

  private constructor(props: SerialNumberProps) {
    super(props);
  }

  public static create(serialNumber: string): Result<SerialNumber> {
    if (Guard.isOneOf(serialNumber, [null, undefined, ''], 'serialNumber').succeeded) {
      return Result.ok<SerialNumber>(new SerialNumber({ value: '' }));
    }

    serialNumber = serialNumber?.trim();

    const guardResult = Guard.combine([Guard.isOfLength(serialNumber, 0, 50, 'serialNumber')]);

    if (!guardResult.succeeded) {
      return Result.fail<SerialNumber>(guardResult.message);
    } else {
      return Result.ok<SerialNumber>(new SerialNumber({ value: serialNumber }));
    }
  }
}
