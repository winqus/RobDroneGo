import { AggregateRoot } from '../../core/domain/AggregateRoot';
import { UniqueEntityID } from '../../core/domain/UniqueEntityID';
import { Guard } from '../../core/logic/Guard';
import { Result } from '../../core/logic/Result';
import { DeliveryTask } from '../DeliveryTask/deliveryTask';
import { SurveillanceTask } from '../SurveillanceTask/surveillanceTask';
import { NavigationData } from './navigationData';
import { TaskStatus } from './taskStatus';

interface TaskProps {
  status: TaskStatus;
  requesterEmail: string;
  task: DeliveryTask | SurveillanceTask;
  requestCreatedDateTime: Date;
  navigationData?: NavigationData;
}

export class TaskRequest extends AggregateRoot<TaskProps> {
  get id(): UniqueEntityID {
    return this._id;
  }

  get requesterEmail(): string {
    return this.props.requesterEmail;
  }

  get task(): DeliveryTask | SurveillanceTask {
    return this.props.task;
  }

  get requestCreatedDateTime(): Date {
    return this.props.requestCreatedDateTime;
  }

  get status(): TaskStatus {
    return this.props.status;
  }

  get navigationData(): NavigationData | null {
    return this.props.navigationData;
  }

  set navigationData(newNavigationData: NavigationData) {
    this.props.navigationData = newNavigationData;
  }

  set status(newStatus: TaskStatus) {
    this.props.status = newStatus;
  }

  private constructor(props: TaskProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create(props: TaskProps, id?: UniqueEntityID): Result<TaskRequest> {
    const guardedProps = [
      { argument: props.requesterEmail, argumentName: 'requesterEmail' },
      { argument: props.requestCreatedDateTime, argumentName: 'requestCreatedDateTime' },
      { argument: props.task, argumentName: 'task' },
      { argument: props.status, argumentName: 'status' },
    ];

    const guardResult = Guard.againstNullOrUndefinedBulk(guardedProps);

    if (!guardResult.succeeded) {
      return Result.fail<TaskRequest>(guardResult.message);
    } else {
      const task = new TaskRequest(
        {
          ...props,
        },
        id,
      );

      return Result.ok<TaskRequest>(task);
    }
  }
}
