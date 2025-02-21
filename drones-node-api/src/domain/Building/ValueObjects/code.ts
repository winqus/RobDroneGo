import { ValueObject } from '../../../core/domain/ValueObject';

import { Guard } from '../../../core/logic/Guard';
import { Result } from '../../../core/logic/Result';

interface CodeProps {
  value: string;
}

export class Code extends ValueObject<CodeProps> {
  get value(): string {
    return this.props.value;
  }

  private constructor(props: CodeProps) {
    super(props);
  }

  public static create(code: string): Result<Code> {
    code = code?.trim();
    const guardResult = Guard.combine([
      Guard.againstNullOrUndefined(code, 'code'),
      Guard.isAlphanumericWithSpaces(code, 'code'),
      Guard.isOfLength(code, 1, 5, 'code'),
    ]);

    if (!guardResult.succeeded) {
      return Result.fail<Code>(guardResult.message);
    } else {
      return Result.ok<Code>(new Code({ value: code }));
    }
  }
}
