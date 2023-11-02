import { ValueObject } from '../../../../core/domain/ValueObject';

import { Guard } from '../../../../core/logic/Guard';
import { Result } from '../../../../core/logic/Result';

interface MakeModelProps {
  make: string;
  model: string;
}

export class MakeModel extends ValueObject<MakeModelProps> {
  get make(): string {
    return this.props.make;
  }

  get model(): string {
    return this.props.model;
  }

  private constructor(props: MakeModelProps) {
    super(props);
  }

  public static create(make: string, model: string): Result<MakeModel> {
    if (
      Guard.isOneOf(make, [null, undefined, ''], 'make').succeeded &&
      Guard.isOneOf(model, [null, undefined, ''], 'model').succeeded
    ) {
      return Result.ok<MakeModel>(new MakeModel({ make: '', model: '' }));
    }

    make = make?.trim();
    model = model?.trim();

    const guardResult = Guard.combine([
      Guard.isOfLength(make, 0, 50, 'make'),
      Guard.isOfLength(model, 0, 50, 'model'),
      Guard.againstNullOrUndefined(make, 'make'),
      Guard.againstNullOrUndefined(model, 'model'),
    ]);
    if (!guardResult.succeeded) {
      return Result.fail<MakeModel>(guardResult.message);
    } else {
      return Result.ok<MakeModel>(new MakeModel({ make: make, model: model }));
    }
  }
}
