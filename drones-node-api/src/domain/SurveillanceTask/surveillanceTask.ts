import { AggregateRoot } from '../../core/domain/AggregateRoot';
import { UniqueEntityID } from '../../core/domain/UniqueEntityID';
import { Guard } from '../../core/logic/Guard';
import { Result } from '../../core/logic/Result';
import { Code } from '../Building/ValueObjects/code';

interface SurveillanceTaskProps {
  buildingCode: Code;
  floorNumber: number[];
  contactNumber: number;
}

export class SurveillanceTask extends AggregateRoot<SurveillanceTaskProps> {
  get buildingCode(): Code {
    return this.props.buildingCode;
  }

  get floorNumber(): number[] {
    return this.props.floorNumber;
  }

  get contactNumber(): number {
    return this.props.contactNumber;
  }

  private constructor(props: SurveillanceTaskProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create(props: SurveillanceTaskProps, id?: UniqueEntityID): Result<SurveillanceTask> {
    const guardedProps = [
      { argument: props.buildingCode, argumentName: 'buildingCode' },
      { argument: props.floorNumber, argumentName: 'floorNumber' },
      { argument: props.contactNumber, argumentName: 'contactNumber' },
    ];

    const guardResult = Guard.againstNullOrUndefinedBulk(guardedProps);

    if (!guardResult.succeeded) {
      return Result.fail<SurveillanceTask>(guardResult.message);
    } else {
      const surveillanceTask = new SurveillanceTask(
        {
          ...props,
        },
        id,
      );

      return Result.ok<SurveillanceTask>(surveillanceTask);
    }
  }
}
