import { ValueObject } from '../../../core/domain/ValueObject';

import { Guard } from '../../../core/logic/Guard';
import { Result } from '../../../core/logic/Result';

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
    serialNumber = serialNumber?.trim();
    const guardResult = Guard.combine([
      Guard.againstNullOrUndefined(serialNumber, 'serialNumber'),
      Guard.isAlphanumericWithSpaces(serialNumber, 'serialNumber'),
      Guard.isOfLength(serialNumber, 1, 50, 'serialNumber'),
    ]);

    if (!guardResult.succeeded) {
      return Result.fail<SerialNumber>(guardResult.message);
    } else {
      return Result.ok<SerialNumber>(new SerialNumber({ value: serialNumber }));
    }
  }
}
