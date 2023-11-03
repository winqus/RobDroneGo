import { UniqueEntityID } from '../../../core/domain/UniqueEntityID';
import { TaskType } from '../taskType';
import { Types } from '../type';

describe('TaskType', () => {
  describe('create', () => {
    it('should successfully create a TaskType instance with valid properties', () => {
      // Arrange
      const taskTypeProps = {
        type: Types.PickUpAndDelivery,
      };

      const id = new UniqueEntityID();

      // Act
      const taskTypeResult = TaskType.create(taskTypeProps, id);

      // Assert
      expect(taskTypeResult.isSuccess).toBe(true);
      const taskType = taskTypeResult.getValue();
      expect(taskType).toBeInstanceOf(TaskType);
      expect(taskType.type).toBe(Types.PickUpAndDelivery);
      expect(taskType.id).toBe(id);
    });

    it('should fail to create a TaskType instance with null type', () => {
      // Arrange
      const taskTypeProps = {
        type: null,
      };

      // Act
      const taskTypeResult = TaskType.create(taskTypeProps);

      // Assert
      expect(taskTypeResult.isFailure).toBe(true);
      expect(taskTypeResult.error).toMatch(/null/);
    });
  });
});
