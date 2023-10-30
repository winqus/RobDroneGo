import { AggregateRoot } from '../../core/domain/AggregateRoot';
import { UniqueEntityID } from '../../core/domain/UniqueEntityID';
import { Guard } from '../../core/logic/Guard';
import { Result } from '../../core/logic/Result';
import { Code as BuildingCode } from '../Building/ValueObjects/code';

interface PassageProps {
  buildingCode1: BuildingCode;
  buildingCode2: BuildingCode;
  floorNumber1: number;
  floorNumber2: number;
}

export class Passage extends AggregateRoot<PassageProps> {
  get id(): UniqueEntityID {
    return this._id;
  }

  get buildingCode1(): BuildingCode {
    return this.props.buildingCode1;
  }

  get buildingCode2(): BuildingCode {
    return this.props.buildingCode2;
  }

  get floorNumber1(): number {
    return this.props.floorNumber1;
  }

  get floorNumber2(): number {
    return this.props.floorNumber2;
  }

  set buildingCode1(value: BuildingCode) {
    this.props.buildingCode1 = value;
  }

  set buildingCode2(value: BuildingCode) {
    this.props.buildingCode2 = value;
  }

  set floorNumber1(value: number) {
    this.props.floorNumber1 = value;
  }

  set floorNumber2(value: number) {
    this.props.floorNumber2 = value;
  }

  private constructor(props: PassageProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create(props: PassageProps, id?: UniqueEntityID): Result<Passage> {
    const guardedProps = [
      { argument: props.buildingCode1, argumentName: 'buildingCode1' },
      { argument: props.buildingCode2, argumentName: 'buildingCode2' },
      { argument: props.floorNumber1, argumentName: 'floorNumber1' },
      { argument: props.floorNumber2, argumentName: 'floorNumber2' },
    ];

    const guardResult = Guard.againstNullOrUndefinedBulk(guardedProps);

    if (!guardResult.succeeded) {
      return Result.fail<Passage>(guardResult.message);
    } else {
      const passage = new Passage(
        {
          ...props,
        },
        id,
      );

      return Result.ok<Passage>(passage);
    }
  }
}
