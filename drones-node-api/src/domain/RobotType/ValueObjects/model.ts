import { ValueObject } from '../../../core/domain/ValueObject';

import { Guard } from '../../../core/logic/Guard';
import { Result } from '../../../core/logic/Result';

interface ModelProps {
  value: string;
}

export class Model extends ValueObject<ModelProps> {
  get value(): string {
    return this.props.value;
  }

  private constructor(props: ModelProps) {
    super(props);
  }

  public static create(model: string): Result<Model> {
    model = model?.trim();
    const guardResult = Guard.combine([
      Guard.againstNullOrUndefined(model, 'model'),
      Guard.isAlphanumeric(model, 'model'),
      Guard.isOfLength(model, 1, 100, 'model'),
    ]);

    if (!guardResult.succeeded) {
      return Result.fail<Model>(guardResult.message);
    } else {
      return Result.ok<Model>(new Model({ value: model }));
    }
  }
}
