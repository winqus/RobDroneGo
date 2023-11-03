import { UniqueEntityID } from '../../../core/domain/UniqueEntityID';
import { TaskType } from '../../TaskType/taskType';
import { Types } from '../../TaskType/type';
import { Brand } from '../ValueObjects/brand';
import { Model } from '../ValueObjects/model';
import { Name } from '../ValueObjects/name';
import { RobotType } from '../robotType'; // Import the RobotType class

describe('RobotType', () => {
  describe('create', () => {
    it('should successfully create a RobotType instance with valid properties', () => {
      // Arrange
      const robotTypeProps = {
        name: Name.create('Robot1').getValue(),
        brand: Brand.create('Brand1').getValue(),
        model: Model.create('Model1').getValue(),
        typesOfTasks: [TaskType.create({ type: Types.PickUpAndDelivery }).getValue()],
      };

      const id = new UniqueEntityID();

      // Act
      const robotTypeResult = RobotType.create(robotTypeProps, id);

      // Assert
      expect(robotTypeResult.isSuccess).toBe(true);
      const robotType = robotTypeResult.getValue();
      expect(robotType).toBeInstanceOf(RobotType);
      expect(robotType.name.value).toBe('Robot1');
      expect(robotType.brand.value).toBe('Brand1');
      expect(robotType.model.value).toBe('Model1');
      expect(robotType.typesOfTasks.length).toBe(1);
      expect(robotType.id).toBe(id);
    });

    it('should fail to create a RobotType instance with null name', () => {
      // Arrange
      const robotTypeProps = {
        name: null,
        brand: Brand.create('Brand1').getValue(),
        model: Model.create('Model1').getValue(),
        typesOfTasks: [TaskType.create({ type: Types.PickUpAndDelivery }).getValue()],
      };

      // Act
      const robotTypeResult = RobotType.create(robotTypeProps);

      // Assert
      expect(robotTypeResult.isFailure).toBe(true);
      expect(robotTypeResult.error).toMatch(/null/);
    });

    it('should fail to create a RobotType instance with null brand', () => {
      // Arrange
      const robotTypeProps = {
        name: Name.create('Robot1').getValue(),
        brand: null,
        model: Model.create('Model1').getValue(),
        typesOfTasks: [TaskType.create({ type: Types.PickUpAndDelivery }).getValue()],
      };

      // Act
      const robotTypeResult = RobotType.create(robotTypeProps);

      // Assert
      expect(robotTypeResult.isFailure).toBe(true);
      expect(robotTypeResult.error).toMatch(/null/);
    });

    it('should fail to create a RobotType instance with null model', () => {
      // Arrange
      const robotTypeProps = {
        name: Name.create('Robot1').getValue(),
        brand: Brand.create('Brand1').getValue(),
        model: null,
        typesOfTasks: [TaskType.create({ type: Types.PickUpAndDelivery }).getValue()],
      };

      // Act
      const robotTypeResult = RobotType.create(robotTypeProps);

      // Assert
      expect(robotTypeResult.isFailure).toBe(true);
      expect(robotTypeResult.error).toMatch(/null/);
    });

    it('should fail to create a RobotType instance with null typesOfTasks', () => {
      // Arrange
      const robotTypeProps = {
        name: Name.create('Robot1').getValue(),
        brand: Brand.create('Brand1').getValue(),
        model: Model.create('Model1').getValue(),
        typesOfTasks: null,
      };

      // Act
      const robotTypeResult = RobotType.create(robotTypeProps);

      // Assert
      expect(robotTypeResult.isFailure).toBe(true);
      expect(robotTypeResult.error).toMatch(/null/);
    });
  });
});
