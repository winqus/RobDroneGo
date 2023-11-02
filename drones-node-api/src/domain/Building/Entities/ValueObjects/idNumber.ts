import { ValueObject } from '../../../../core/domain/ValueObject';

import { Guard } from '../../../../core/logic/Guard';
import { Result } from '../../../../core/logic/Result';

interface IDNumberProps {
  value: number;
}

export class IDNumber extends ValueObject<IDNumberProps> {
  get value(): number {
    return this.props.value;
  }

  private constructor(props: IDNumberProps) {
    super(props);
  }

  public static create(idNumber: number): Result<IDNumber> {
    const guardResult = Guard.isInteger(idNumber, 'idNumber');

    if (!guardResult.succeeded) {
      return Result.fail<IDNumber>(guardResult.message);
    } else {
      return Result.ok<IDNumber>(new IDNumber({ value: idNumber }));
    }
  }
}
