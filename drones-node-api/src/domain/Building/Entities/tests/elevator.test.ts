import { UniqueEntityID } from '../../../../core/domain/UniqueEntityID';
import { Description } from '../ValueObjects/description';
import { IDNumber } from '../ValueObjects/idNumber';
import { MakeModel } from '../ValueObjects/makeModel';
import { SerialNumber } from '../ValueObjects/serialNumber';
import { Elevator } from '../elevator';

describe('Elevator', () => {
  describe('create', () => {
    it('should successfully create an Elevator instance with valid properties', () => {
      // Arrange
      const elevatorProps = {
        number: IDNumber.create(1).getValue(),
        makeModel: MakeModel.create('make1', 'Model1').getValue(),
        serialNumber: SerialNumber.create('Serial1').getValue(),
        description: Description.create('Description1').getValue(),
      };

      const id = new UniqueEntityID();

      // Act
      const elevatorResult = Elevator.create(elevatorProps, id);

      // Assert
      expect(elevatorResult.isSuccess).toBe(true);
      const elevator = elevatorResult.getValue();
      expect(elevator).toBeInstanceOf(Elevator);
      expect(elevator.number.value).toBe(1);
      expect(elevator.makeModel.make).toBe('make1');
      expect(elevator.makeModel.model).toBe('Model1');
      expect(elevator.serialNumber.value).toBe('Serial1');
      expect(elevator.description.value).toBe('Description1');
      expect(elevator.id).toBe(id);
    });

    it('should successfully create an Elevator instance with default description', () => {
      // Arrange
      const elevatorProps = {
        number: IDNumber.create(1).getValue(),
        makeModel: MakeModel.create('make1', 'Model1').getValue(),
        serialNumber: SerialNumber.create('Serial1').getValue(),
        description: null,
      };

      const id = new UniqueEntityID();

      // Act
      const elevatorResult = Elevator.create(elevatorProps, id);

      // Assert
      expect(elevatorResult.isSuccess).toBe(true);
      const elevator = elevatorResult.getValue();
      expect(elevator).toBeInstanceOf(Elevator);
      expect(elevator.number.value).toBe(1);
      expect(elevator.makeModel.make).toBe('make1');
      expect(elevator.makeModel.model).toBe('Model1');
      expect(elevator.serialNumber.value).toBe('Serial1');
      expect(elevator.description.value).toBe('');
      expect(elevator.id).toBe(id);
    });

    it('should fail to create an Elevator instance with null number', () => {
      // Arrange
      const elevatorProps = {
        number: null,
        makeModel: MakeModel.create('make1', 'Model1').getValue(),
        serialNumber: SerialNumber.create('Serial1').getValue(),
        description: Description.create('Description1').getValue(),
      };

      // Act
      const elevatorResult = Elevator.create(elevatorProps);

      // Assert
      expect(elevatorResult.isFailure).toBe(true);
      expect(elevatorResult.error).toMatch(/null/);
    });
    it('should successfully create an Elevator instance with minimal required properties', () => {
      // Arrange
      const elevatorProps = {
        number: IDNumber.create(1).getValue(),
      };

      const id = new UniqueEntityID();

      // Act
      const elevatorResult = Elevator.create(elevatorProps, id);

      // Assert
      expect(elevatorResult.isSuccess).toBe(true);
      const elevator = elevatorResult.getValue();
      expect(elevator).toBeInstanceOf(Elevator);
      expect(elevator.number.value).toBe(1);
      expect(elevator.makeModel).toBeUndefined();
      expect(elevator.serialNumber).toBeUndefined();
      expect(elevator.description.value).toBe('');
      expect(elevator.id).toBe(id);
    });

    it('should successfully create an Elevator instance with default description and optional properties', () => {
      // Arrange
      const elevatorProps = {
        number: IDNumber.create(1).getValue(),
        makeModel: MakeModel.create('make1', 'Model1').getValue(),
        serialNumber: SerialNumber.create('Serial1').getValue(),
        description: null,
      };

      const id = new UniqueEntityID();

      // Act
      const elevatorResult = Elevator.create(elevatorProps, id);

      // Assert
      expect(elevatorResult.isSuccess).toBe(true);
      const elevator = elevatorResult.getValue();
      expect(elevator).toBeInstanceOf(Elevator);
      expect(elevator.number.value).toBe(1);
      expect(elevator.makeModel.make).toBe('make1');
      expect(elevator.makeModel.model).toBe('Model1');
      expect(elevator.serialNumber.value).toBe('Serial1');
      expect(elevator.description.value).toBe('');
      expect(elevator.id).toBe(id);
    });
  });
});
