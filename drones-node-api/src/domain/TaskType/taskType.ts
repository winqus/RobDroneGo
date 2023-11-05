import { AggregateRoot } from '../../core/domain/AggregateRoot';
import { UniqueEntityID } from '../../core/domain/UniqueEntityID';
import { Guard } from '../../core/logic/Guard';
import { Result } from '../../core/logic/Result';
import { Types } from './type';

interface TaskTypePros {
  type: Types;
}

export class TaskType extends AggregateRoot<TaskTypePros> {
  get id(): UniqueEntityID {
    return this._id;
  }

  get type(): Types {
    return this.props.type;
  }

  private constructor(props: TaskTypePros, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create(props: TaskTypePros, id?: UniqueEntityID): Result<TaskType> {
    const guardedProps = [{ argument: props.type, argumentName: 'type' }];

    const guardResult = Guard.againstNullOrUndefinedBulk(guardedProps);

    if (!guardResult.succeeded) {
      return Result.fail<TaskType>(guardResult.message);
    } else {
      const robotType = new TaskType(
        {
          ...props,
        },
        id,
      );

      return Result.ok<TaskType>(robotType);
    }
  }
}
