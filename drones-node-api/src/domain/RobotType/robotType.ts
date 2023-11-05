import { AggregateRoot } from '../../core/domain/AggregateRoot';
import { UniqueEntityID } from '../../core/domain/UniqueEntityID';
import { Guard } from '../../core/logic/Guard';
import { Result } from '../../core/logic/Result';
import { TaskType } from '../TaskType/taskType';
import { Brand } from './ValueObjects/brand';
import { Model } from './ValueObjects/model';
import { Name } from './ValueObjects/name';

interface RobotTypeProps {
  name: Name;
  brand: Brand;
  model: Model;
  typesOfTasks: TaskType[];
}

export class RobotType extends AggregateRoot<RobotTypeProps> {
  get id(): UniqueEntityID {
    return this._id;
  }

  get name(): Name {
    return this.props.name;
  }

  set name(value: Name) {
    this.props.name = value;
  }

  get brand(): Brand {
    return this.props.brand;
  }

  get model(): Model {
    return this.props.model;
  }

  get typesOfTasks(): TaskType[] {
    return this.props.typesOfTasks;
  }

  private constructor(props: RobotTypeProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create(props: RobotTypeProps, id?: UniqueEntityID): Result<RobotType> {
    const guardedProps = [
      { argument: props.name, argumentName: 'name' },
      { argument: props.brand, argumentName: 'brand' },
      { argument: props.model, argumentName: 'model' },
      { argument: props.typesOfTasks, argumentName: 'typesOfTasks' },
    ];

    const guardResult = Guard.againstNullOrUndefinedBulk(guardedProps);

    if (!guardResult.succeeded) {
      return Result.fail<RobotType>(guardResult.message);
    } else {
      const robotType = new RobotType(
        {
          ...props,
        },
        id,
      );

      return Result.ok<RobotType>(robotType);
    }
  }
}
