import { ValueObject } from '../../../core/domain/ValueObject';

import { Guard } from '../../../core/logic/Guard';
import { Result } from '../../../core/logic/Result';

interface BrandProps {
  value: string;
}

export class Brand extends ValueObject<BrandProps> {
  get value(): string {
    return this.props.value;
  }

  private constructor(props: BrandProps) {
    super(props);
  }

  public static create(brand: string): Result<Brand> {
    brand = brand?.trim();
    const guardResult = Guard.combine([
      Guard.againstNullOrUndefined(brand, 'brand'),
      Guard.isAlphanumeric(brand, 'brand'),
      Guard.isOfLength(brand, 1, 50, 'brand'),
    ]);

    if (!guardResult.succeeded) {
      return Result.fail<Brand>(guardResult.message);
    } else {
      return Result.ok<Brand>(new Brand({ value: brand }));
    }
  }
}
