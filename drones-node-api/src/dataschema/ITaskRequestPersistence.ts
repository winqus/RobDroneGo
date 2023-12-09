import { IDeliveryTaskPersistence } from './IDeliveryTaskPersistence';
import { INavigationDataPersistence } from './INavigationDataPersistence';
import { ISurveillanceTaskPersistence } from './ISurveillanceTaskPersistence';

export interface ITaskRequestPersistence {
  id: string;
  status: string;
  requesterEmail: string;
  task: IDeliveryTaskPersistence | ISurveillanceTaskPersistence;
  requestCreatedDateTime: string;
  navigationData?: INavigationDataPersistence;
}
