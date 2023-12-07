interface ITaskRequestDTO {
  id: string;
  status: string;
  requesterEmail: string;
  task: IDeliveryTaskDTO | ISurveillanceTaskDTO;
  requestCreatedDateTime: Date;
  navigationData?: INavigationDataDTO;
}
