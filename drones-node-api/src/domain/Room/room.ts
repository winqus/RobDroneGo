import { AggregateRoot } from '../../core/domain/AggregateRoot';
import { UniqueEntityID } from '../../core/domain/UniqueEntityID';
import { Guard } from '../../core/logic/Guard';
import { Result } from '../../core/logic/Result';
import { Description } from '../Building/ValueObjects/description';
import { FloorSize as Size } from '../Building/ValueObjects/floorSize';
import { PositionOnMap as Position } from '../common/positionOnMap';
import { RoomCategory } from './ValueObjects/category';
import { Name } from './ValueObjects/name';

interface RoomProps {
  name: Name;
  description?: Description;
  category: RoomCategory;
  size: Size;
  position: Position;
  floorId: UniqueEntityID;
}

export class Room extends AggregateRoot<RoomProps> {
  get id(): UniqueEntityID {
    return this._id;
  }

  get name(): Name {
    return this.props.name;
  }

  set name(value: Name) {
    this.props.name = value;
  }

  get description(): Description {
    return this.props.description;
  }

  set description(value: Description) {
    this.props.description = value;
  }

  get size(): Size {
    return this.props.size;
  }

  set size(value: Size) {
    this.props.size = value;
  }

  get position(): Position {
    return this.props.position;
  }

  set position(value: Position) {
    this.props.position = value;
  }

  get category(): RoomCategory {
    return this.props.category;
  }

  set category(value: RoomCategory) {
    this.props.category = value;
  }

  get floorId(): UniqueEntityID {
    return this.props.floorId;
  }

  private constructor(props: RoomProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create(props: RoomProps, id?: UniqueEntityID): Result<Room> {
    const guardedProps = [
      { argument: props.name, argumentName: 'name' },
      { argument: props.size, argumentName: 'size' },
      { argument: props.position, argumentName: 'position' },
      { argument: props.category, argumentName: 'category' },
      { argument: props.floorId, argumentName: 'floorId' },
    ];

    const guardResult = Guard.againstNullOrUndefinedBulk(guardedProps);

    if (!guardResult.succeeded) {
      return Result.fail<Room>(guardResult.message);
    } else {
      const room = new Room(
        {
          ...props,
        },
        id,
      );

      return Result.ok<Room>(room);
    }
  }
}
