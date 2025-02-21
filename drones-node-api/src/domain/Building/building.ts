import { AggregateRoot } from '../../core/domain/AggregateRoot';
import { UniqueEntityID } from '../../core/domain/UniqueEntityID';
import { Guard } from '../../core/logic/Guard';
import { Result } from '../../core/logic/Result';
import { Elevator } from './Entities/elevator';
import { Code } from './ValueObjects/code';
import { Description } from './ValueObjects/description';
import { FloorSize } from './ValueObjects/floorSize';
import { Name } from './ValueObjects/name';

interface BuildingProps {
  name: Name | null;
  code: Code;
  description: Description;
  floorSize: FloorSize;
  elevator?: Elevator;
}

export class Building extends AggregateRoot<BuildingProps> {
  get id(): UniqueEntityID {
    return this._id;
  }

  get name(): Name {
    return this.props.name;
  }

  set name(value: Name) {
    this.props.name = value;
  }

  get code(): Code {
    return this.props.code;
  }

  set description(value: Description) {
    this.props.description = value;
  }

  get description(): Description {
    return this.props.description;
  }

  set floorSize(value: FloorSize) {
    this.props.floorSize = value;
  }

  get floorSize(): FloorSize {
    return this.props.floorSize;
  }

  get elevator(): Elevator {
    return this.props.elevator;
  }

  set elevator(value: Elevator) {
    this.props.elevator = value;
  }

  private constructor(props: BuildingProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create(props: BuildingProps, id?: UniqueEntityID): Result<Building> {
    const guardedProps = [
      { argument: props.code, argumentName: 'code' },
      { argument: props.description, argumentName: 'description' },
      { argument: props.floorSize, argumentName: 'floorSize' },
    ];

    const guardResult = Guard.againstNullOrUndefinedBulk(guardedProps);

    if (!guardResult.succeeded) {
      return Result.fail<Building>(guardResult.message);
    } else {
      const building = new Building(
        {
          ...props,
        },
        id,
      );

      return Result.ok<Building>(building);
    }
  }
}
