import { DeliveryTask } from './deliveryTask.model';
import { NavigationPlan } from './shared/navigationPlan.interface';
import { SurveillanceTask } from './surveillanceTask.model';

export interface ITaskRequestDTO {
  id: string;
  status: string;
  requesterEmail: string;
  task: DeliveryTask | SurveillanceTask;
  requestCreatedDateTime: string;
  navigationData?: NavigationPlan;
}
