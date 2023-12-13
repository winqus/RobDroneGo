import { TaskRequest } from '../models/taskRequest.model';

export function isDeliveryTask(taskRequest: TaskRequest): boolean {
  return 'pickUpRoomId' in taskRequest.task && 'deliveryRoomId' in taskRequest.task;
}

export function isSurveillanceTask(taskRequest: TaskRequest): boolean {
  return 'buildingCode' in taskRequest.task && 'floorNumber' in taskRequest.task && 'contactNumber' in taskRequest.task;
}
