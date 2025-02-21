import { ValueObject } from '../../../core/domain/ValueObject';

import { Guard } from '../../../core/logic/Guard';
import { Result } from '../../../core/logic/Result';

interface NicknameProps {
  value: string;
}

export class Nickname extends ValueObject<NicknameProps> {
  get value(): string {
    return this.props.value;
  }

  private constructor(props: NicknameProps) {
    super(props);
  }

  public static create(nickname: string): Result<Nickname> {
    nickname = nickname?.trim();
    const guardResult = Guard.combine([
      Guard.againstNullOrUndefined(nickname, 'nickname'),
      Guard.isAlphanumeric(nickname, 'nickname'),
      Guard.isOfLength(nickname, 1, 30, 'nickname'),
    ]);

    if (!guardResult.succeeded) {
      return Result.fail<Nickname>(guardResult.message);
    } else {
      return Result.ok<Nickname>(new Nickname({ value: nickname }));
    }
  }
}
