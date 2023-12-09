import { IDeliveryTaskDTO } from './IDeliveryTaskDTO';
import { ISurveillanceTaskDTO } from './ISurveillanceTaskDTO';

export interface ITaskRequestDTO {
  id: string;
  status: string;
  requesterEmail: string;
  task: IDeliveryTaskDTO | ISurveillanceTaskDTO;
  requestCreatedDateTime: string;
  navigationData?: INavigationDataDTO;
}
