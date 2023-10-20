import { ValueObject } from '../../../core/domain/ValueObject';

import { Result } from '../../../core/logic/Result';
import { Guard } from '../../../core/logic/Guard';

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
    code = code.trim();
    const guardResult = Guard.combine([
      Guard.againstNullOrUndefined(code, 'name'),
      Guard.isAlphanumericWithSpaces(code, 'name'),
      Guard.isOfLength(code, 1, 5, 'name'),
    ]);

    if (!guardResult.succeeded) {
      return Result.fail<Code>(guardResult.message);
    } else {
      return Result.ok<Code>(new Code({ value: code }));
    }
  }
}
