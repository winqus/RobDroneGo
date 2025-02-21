import { UniqueEntityID } from '../core/domain/UniqueEntityID';
import { Mapper } from '../core/infra/Mapper';
import { Result } from '../core/logic/Result';
import { IDeliveryTaskPersistence } from '../dataschema/IDeliveryTaskPersistence';
import { DeliveryTask } from '../domain/DeliveryTask/deliveryTask';
import { IDeliveryTaskDTO } from '../dto/IDeliveryTaskDTO';

export class DeliveryTaskMap extends Mapper<DeliveryTask> {
  public static toDTO(deliveryTask: DeliveryTask): IDeliveryTaskDTO {
    return {
      id: deliveryTask.id.toString(),
      pickUpRoomId: deliveryTask.pickUpRoomId.toString(),
      deliveryRoomId: deliveryTask.deliveryRoomId.toString(),
      pickUpContact: deliveryTask.pickUpContact,
      pickUpName: deliveryTask.pickUpName,
      deliveryContact: deliveryTask.deliveryContact,
      deliveryName: deliveryTask.deliveryName,
      confirmationCode: deliveryTask.confirmationCode,
      description: deliveryTask.description,
    } as IDeliveryTaskDTO;
  }

  public static toDomain(raw: any): Result<DeliveryTask> {
    const pickUpRoomId = new UniqueEntityID(raw?.pickUpRoomId);
    const deliveryRoomId = new UniqueEntityID(raw?.deliveryRoomId);
    const pickUpContact = raw?.pickUpContact;
    const pickUpName = raw?.pickUpName;
    const deliveryContact = raw?.deliveryContact;
    const deliveryName = raw?.deliveryName;
    const confirmationCode = raw?.confirmationCode;
    const description = raw?.description;

    const deliveryTaskOrError = DeliveryTask.create(
      {
        pickUpRoomId: pickUpRoomId,
        deliveryRoomId: deliveryRoomId,
        pickUpContact: pickUpContact,
        pickUpName: pickUpName,
        deliveryContact: deliveryContact,
        deliveryName: deliveryName,
        confirmationCode: confirmationCode,
        description: description,
      },
      new UniqueEntityID(raw.id),
    );

    deliveryTaskOrError.isFailure ? console.log(deliveryTaskOrError.error) : '';

    return deliveryTaskOrError;
  }

  public static toPersistence(deliveryTask: DeliveryTask): IDeliveryTaskPersistence {
    return {
      id: deliveryTask.id.toString(),
      pickUpRoomId: deliveryTask.pickUpRoomId.toString(),
      deliveryRoomId: deliveryTask.deliveryRoomId.toString(),
      pickUpContact: deliveryTask.pickUpContact,
      pickUpName: deliveryTask.pickUpName,
      deliveryContact: deliveryTask.deliveryContact,
      deliveryName: deliveryTask.deliveryName,
      confirmationCode: deliveryTask.confirmationCode,
      description: deliveryTask.description,
    } as IDeliveryTaskPersistence;
  }
}
