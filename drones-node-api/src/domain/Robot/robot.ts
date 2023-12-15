import { AggregateRoot } from '../../core/domain/AggregateRoot';
import { UniqueEntityID } from '../../core/domain/UniqueEntityID';
import { Guard } from '../../core/logic/Guard';
import { Result } from '../../core/logic/Result';
import { Name as RobotType } from '../RobotType/ValueObjects/name';
import { Code } from './ValueObjects/code';
import { Description } from './ValueObjects/description';
import { Nickname } from './ValueObjects/nickname';
import { SerialNumber } from './ValueObjects/serialNumber';

export interface Position {
  floorNumber: number;
  buildingCode: string;
  cellPosition: [number, number];
}
interface RobotProps {
  code: Code;
  description: Description | null;
  nickname: Nickname;
  serialNumber: SerialNumber;
  available: boolean;
  type: RobotType;
  position: Position;
}

export class Robot extends AggregateRoot<RobotProps> {
  get id(): UniqueEntityID {
    return this._id;
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

  get nickname(): Nickname {
    return this.props.nickname;
  }

  get serialNumber(): SerialNumber {
    return this.props.serialNumber;
  }

  get available(): boolean {
    return this.props.available;
  }

  set available(value: boolean) {
    this.props.available = value;
  }

  get type(): RobotType {
    return this.props.type;
  }

  get position(): Position {
    return this.props.position;
  }

  set position(value: Position) {
    this.props.position = value;
  }

  private constructor(props: RobotProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create(props: RobotProps, id?: UniqueEntityID): Result<Robot> {
    const guardedProps = [
      { argument: props.code, argumentName: 'code' },
      { argument: props.nickname, argumentName: 'nickname' },
      { argument: props.serialNumber, argumentName: 'serialNumber' },
      { argument: props.type, argumentName: 'type' },
      { argument: props.position, argumentName: 'position' },
    ];

    const guardResult = Guard.againstNullOrUndefinedBulk(guardedProps);

    if (!guardResult.succeeded) {
      return Result.fail<Robot>(guardResult.message);
    } else {
      const robot = new Robot(
        {
          ...props,
        },
        id,
      );

      return Result.ok<Robot>(robot);
    }
  }
}
