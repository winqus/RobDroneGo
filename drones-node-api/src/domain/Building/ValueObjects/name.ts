import { ValueObject } from '../../../core/domain/ValueObject';

import { Result } from '../../../core/logic/Result';
import { Guard } from '../../../core/logic/Guard';

interface NameProps {
  value: string;
}

export class Name extends ValueObject<NameProps> {
  get value(): string {
    return this.props.value;
  }

  private constructor(props: NameProps) {
    super(props);
  }

  public static create(name: string): Result<Name> {
    const guardResult = Guard.combine([Guard.isAlphanumeric(name, 'name'), Guard.isOfLength(name, 0, 50, 'name')]);

    if (!guardResult.succeeded) {
      return Result.fail<Name>(guardResult.message);
    } else {
      return Result.ok<Name>(new Name({ value: name }));
    }
  }
}
