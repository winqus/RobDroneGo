export interface IDeliveryTaskPersistence {
  pickUpRoomId: string;
  deliveryRoomId: string;
  pickUpContact: number;
  pickUpName: string;
  deliveryContact: number;
  deliveryName: string;
  confirmationCode: number;
  description: string;
}
