import { ValueObject } from '../../../../core/domain/ValueObject';

import { Guard } from '../../../../core/logic/Guard';
import { Result } from '../../../../core/logic/Result';

interface DescriptionProps {
  value: string;
}

export class Description extends ValueObject<DescriptionProps> {
  get value(): string {
    return this.props.value;
  }

  private constructor(props: DescriptionProps) {
    super(props);
  }

  public static create(description: string): Result<Description> {
    if (Guard.isOneOf(description, [null, undefined, ''], 'description').succeeded) {
      return Result.ok<Description>(new Description({ value: '' }));
    }

    description = description?.trim();

    const guardResult = Guard.combine([Guard.isOfLength(description, 0, 250, 'description')]);

    if (!guardResult.succeeded) {
      return Result.fail<Description>(guardResult.message);
    } else {
      return Result.ok<Description>(new Description({ value: description }));
    }
  }
}
