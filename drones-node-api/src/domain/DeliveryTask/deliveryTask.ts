import { AggregateRoot } from '../../core/domain/AggregateRoot';
import { UniqueEntityID } from '../../core/domain/UniqueEntityID';
import { Guard } from '../../core/logic/Guard';
import { Result } from '../../core/logic/Result';

interface DeliveryTaskProps {
  pickUpRoomId: UniqueEntityID;
  deliveryRoomId: UniqueEntityID;
  pickUpContact: number;
  pickUpName: string;
  deliveryContact: number;
  deliveryName: string;
  confirmationCode: number;
  description: string;
}

export class DeliveryTask extends AggregateRoot<DeliveryTaskProps> {
  get pickUpRoomId(): UniqueEntityID {
    return this.props.pickUpRoomId;
  }

  get deliveryRoomId(): UniqueEntityID {
    return this.props.deliveryRoomId;
  }

  get pickUpContact(): number {
    return this.props.pickUpContact;
  }

  get pickUpName(): string {
    return this.props.pickUpName;
  }

  get deliveryContact(): number {
    return this.props.deliveryContact;
  }

  get deliveryName(): string {
    return this.props.deliveryName;
  }

  get confirmationCode(): number {
    return this.props.confirmationCode;
  }

  get description(): string {
    return this.props.description;
  }

  private constructor(props: DeliveryTaskProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create(props: DeliveryTaskProps, id?: UniqueEntityID): Result<DeliveryTask> {
    const guardedProps = [
      { argument: props.pickUpRoomId, argumentName: 'pickUpRoomId' },
      { argument: props.deliveryRoomId, argumentName: 'deliveryRoomId' },
      { argument: props.pickUpContact, argumentName: 'pickUpContact' },
      { argument: props.pickUpName, argumentName: 'pickUpName' },
      { argument: props.deliveryContact, argumentName: 'deliveryContact' },
      { argument: props.deliveryName, argumentName: 'deliveryName' },
      { argument: props.confirmationCode, argumentName: 'confirmationCode' },
      { argument: props.description, argumentName: 'description' },
    ];

    const guardResult = Guard.againstNullOrUndefinedBulk(guardedProps);

    if (!guardResult.succeeded) {
      return Result.fail<DeliveryTask>(guardResult.message);
    } else {
      const task = new DeliveryTask(
        {
          ...props,
        },
        id,
      );

      return Result.ok<DeliveryTask>(task);
    }
  }
}
