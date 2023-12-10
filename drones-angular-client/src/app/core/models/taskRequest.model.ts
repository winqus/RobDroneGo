import { DeliveryTask } from './deliveryTask.model';
import { NavigationPlan } from './shared/navigationPlan.interface';
import { SurveillanceTask } from './surveillanceTask.model';

export enum TaskStatus {
  Pending,
  Approved,
  In_Execution,
  Completed,
  Denied,
}

export interface TaskRequest {
  id: string;
  status: TaskStatus;
  requesterEmail: string;
  task: DeliveryTask | SurveillanceTask;
  requestCreatedDateTime: string;
  navigationData?: NavigationPlan;
}
