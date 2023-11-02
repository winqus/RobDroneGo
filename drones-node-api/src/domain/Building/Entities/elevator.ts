import { Entity } from '../../../core/domain/Entity';
import { UniqueEntityID } from '../../../core/domain/UniqueEntityID';

import { Guard } from '../../../core/logic/Guard';
import { Result } from '../../../core/logic/Result';
import { Description } from './ValueObjects/description';
import { IDNumber } from './ValueObjects/idNumber';
import { MakeModel } from './ValueObjects/makeModel';
import { SerialNumber } from './ValueObjects/serialNumber';

interface ElevatorProps {
  number: IDNumber;
  makeModel?: MakeModel;
  serialNumber?: SerialNumber;
  description?: Description;
}

export class Elevator extends Entity<ElevatorProps> {
  get id(): UniqueEntityID {
    return this._id;
  }

  get number(): IDNumber {
    return this.props.number;
  }

  get makeModel(): MakeModel {
    return this.props.makeModel;
  }

  get serialNumber(): SerialNumber {
    return this.props.serialNumber;
  }

  get description(): Description {
    return this.props.description;
  }

  private constructor(props: ElevatorProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create(props: ElevatorProps, id?: UniqueEntityID): Result<Elevator> {
    const guardedProps = [{ argument: props.number, argumentName: 'number' }];

    const guardResult = Guard.againstNullOrUndefinedBulk(guardedProps);

    if (!guardResult.succeeded) {
      return Result.fail<Elevator>(guardResult.message);
    } else {
      const elevator = new Elevator(
        {
          ...props,
          description: props.description ? props.description : Description.create('').getValue(),
        },
        id,
      );

      return Result.ok<Elevator>(elevator);
    }
  }
}
