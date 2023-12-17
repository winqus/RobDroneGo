import { DeliveryTask } from 'src/app/core/models/deliveryTask.model';
import { TaskRequest, TaskStatus } from 'src/app/core/models/taskRequest.model';
import NAVIGATION_DATA_EXAMPLE from './navigationData2.example.json';

const mockedDeliveryTaskRequest: TaskRequest = {
  id: 'some-id',
  status: TaskStatus.Planned,
  requesterEmail: 'some-email@mail.e',
  task: {
    pickUpRoomId: 'some-pickup-room-id',
    deliveryRoomId: 'some-delivery-room-id',
    pickUpContact: 123456789,
    pickUpName: 'some-pickup-name',
    deliveryContact: 987654321,
    deliveryName: 'some-delivery-name',
    confirmationCode: 1234,
    description: 'some-description',
  } as DeliveryTask,
  requestCreatedDateTime: new Date().toISOString(),
  navigationData: NAVIGATION_DATA_EXAMPLE,
};

export default mockedDeliveryTaskRequest;
