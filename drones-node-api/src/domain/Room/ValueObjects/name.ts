import { ValueObject } from '../../../core/domain/ValueObject';
import { Guard } from '../../../core/logic/Guard';
import { Result } from '../../../core/logic/Result';

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
    if (Guard.isOneOf(name, [null, undefined, ''], 'name').succeeded) {
      return Result.fail<Name>('Name should not be empty');
    }

    name = name.trim();
    const guardResult = Guard.combine([
      Guard.isAlphanumericWithSpaces(name, 'name'),
      Guard.isOfLength(name, 1, 50, 'name'),
    ]);

    if (!guardResult.succeeded) {
      return Result.fail<Name>(guardResult.message);
    } else {
      return Result.ok<Name>(new Name({ value: name }));
    }
  }
}
