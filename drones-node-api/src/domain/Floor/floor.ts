import { AggregateRoot } from '../../core/domain/AggregateRoot';
import { UniqueEntityID } from '../../core/domain/UniqueEntityID';
import { Guard } from '../../core/logic/Guard';
import { Result } from '../../core/logic/Result';
import { Code as BuildingCode } from '../Building/ValueObjects/code';
import { Description } from '../Building/ValueObjects/description';

interface FloorProps {
  code: string;
  description: Description | null;
  servedByElevator: boolean;
  buildingCode: BuildingCode;
}

export class Floor extends AggregateRoot<FloorProps> {
  get id(): UniqueEntityID {
    return this._id;
  }

  get code(): string {
    return this.props.code;
  }

  get description(): Description {
    return this.props.description;
  }

  get buildingCode(): BuildingCode {
    return this.props.buildingCode;
  }

  get servedByElevator(): boolean {
    return this.props.servedByElevator;
  }

  set code(value: string) {
    this.props.code = value;
  }

  set description(value: Description) {
    this.props.description = value;
  }

  set servedByElevator(value: boolean) {
    this.props.servedByElevator = value;
  }

  private constructor(props: FloorProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create(props: FloorProps, id?: UniqueEntityID): Result<Floor> {
    const guardedProps = [
      { argument: props.code, argumentName: 'code' },
      { argument: props.servedByElevator, argumentName: 'servedByElevator' },
      { argument: props.buildingCode, argumentName: 'buildingCode' },
    ];

    const guardResult = Guard.combine([
      Guard.againstNullOrUndefinedBulk(guardedProps),
      Guard.isNotEmpty(props.code, 'code'),
    ]);

    if (!guardResult.succeeded) {
      return Result.fail<Floor>(guardResult.message);
    } else {
      const floor = new Floor(
        {
          ...props,
        },
        id,
      );

      return Result.ok<Floor>(floor);
    }
  }
}
