import { UniqueEntityID } from '../core/domain/UniqueEntityID';
import { Mapper } from '../core/infra/Mapper';
import { IDNumber } from '../domain/Building/Entities/ValueObjects/idNumber';
import { MakeModel } from '../domain/Building/Entities/ValueObjects/makeModel';
import { SerialNumber } from '../domain/Building/Entities/ValueObjects/serialNumber';
import { Elevator } from '../domain/Building/Entities/elevator'; // Import the Elevator entity
import { Description } from '../domain/Building/ValueObjects/description';
import IElevatorDTO from '../dto/IElevatorDTO';

export class ElevatorMap extends Mapper<Elevator> {
  public static toDTO(elevator: Elevator): IElevatorDTO {
    return {
      id: elevator.id.toString(),
      number: elevator.number.value,
      make: elevator.makeModel ? elevator.makeModel.make : null,
      model: elevator.makeModel ? elevator.makeModel.model : null,
      serialNumber: elevator.serialNumber ? elevator.serialNumber.value : null,
      description: elevator.description ? elevator.description.value : null,
    };
  }

  public static toDomain(raw: IElevatorDTO): Elevator {
    const number = IDNumber.create(raw.number).getValue();
    const makeModel = raw.make !== null && raw.model !== null ? MakeModel.create(raw.make, raw.model).getValue() : null;
    const serialNumber = raw.serialNumber ? SerialNumber.create(raw.serialNumber).getValue() : null;
    const description = raw.description ? Description.create(raw.description).getValue() : null;

    const elevatorOrError = Elevator.create(
      {
        number,
        makeModel,
        serialNumber,
        description,
      },
      new UniqueEntityID(raw.id),
    );

    elevatorOrError.isFailure ? console.log(elevatorOrError.error) : '';

    return elevatorOrError.isSuccess ? elevatorOrError.getValue() : null;
  }

  public static toPersistence(elevator: Elevator): IElevatorDTO {
    return {
      id: elevator.id.toString(),
      number: elevator.number.value,
      make: elevator.makeModel ? elevator.makeModel.make : null,
      model: elevator.makeModel ? elevator.makeModel.model : null,
      serialNumber: elevator.serialNumber ? elevator.serialNumber.value : null,
      description: elevator.description ? elevator.description.value : null,
    };
  }
}
