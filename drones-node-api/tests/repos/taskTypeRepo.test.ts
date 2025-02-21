import { mock, MockProxy } from 'jest-mock-extended';
import { Document, Model } from 'mongoose';
import { UniqueEntityID } from '../../src/core/domain/UniqueEntityID';
import { ITaskTypePersistence } from '../../src/dataschema/ITaskTypePersistence';
import { TaskType } from '../../src/domain/TaskType/taskType';
import { Types } from '../../src/domain/TaskType/type';
import { TaskTypeMap } from '../../src/mappers/TaskTypeMap';
import TaskTypeRepo from '../../src/repos/taskTypeRepo';

describe('TaskTypeRepo', () => {
  let taskTypeRepo: TaskTypeRepo;
  type TaskTypeDocument = ITaskTypePersistence & Document;
  let taskTypeSchemaMock: MockProxy<Model<TaskTypeDocument>> & Model<TaskTypeDocument>;

  beforeEach(() => {
    taskTypeSchemaMock = mock<Model<TaskTypeDocument>>();
    taskTypeSchemaMock.findOne.mockResolvedValue(null as any);
    taskTypeRepo = new TaskTypeRepo(taskTypeSchemaMock);
  });

  it('should save a new task type', async () => {
    // Arrange
    const taskType = TaskType.create({ type: Types.PickUpAndDelivery }).getValue();
    const taskTypePersistence = TaskTypeMap.toPersistence(taskType);

    taskTypeSchemaMock.create.mockResolvedValueOnce(taskTypePersistence as any);

    // Act
    const result = await taskTypeRepo.save(taskType);

    // Assert
    expect(result).toEqual(taskType);
    expect(taskTypeSchemaMock.create).toBeCalledWith(taskTypePersistence);
  });

  it('should update a task type', async () => {
    // Arrange
    const taskType = TaskType.create({ type: Types.PickUpAndDelivery }).getValue();
    const taskTypePersistence = TaskTypeMap.toPersistence(taskType) as TaskTypeDocument;

    taskTypeSchemaMock.findOne.mockResolvedValueOnce(taskTypePersistence as any);
    taskTypePersistence.save = jest.fn().mockResolvedValueOnce({ nModified: 1 });

    // Act
    const result = await taskTypeRepo.save(taskType);

    // Assert
    expect(result).toEqual(taskType);
    expect(taskTypeSchemaMock.findOne).toBeCalledWith({ id: taskType.id.toString() });
    expect(taskTypePersistence.save).toBeCalled();
  });

  it('should find a task type by id', async () => {
    // Arrange
    const taskTypeId = new UniqueEntityID().toString();
    const taskType = TaskType.create({ type: Types.PickUpAndDelivery }).getValue();
    const taskTypePersistence = TaskTypeMap.toPersistence(taskType);

    taskTypeSchemaMock.findOne.mockResolvedValueOnce(taskTypePersistence as any);

    // Act
    const foundTaskType = await taskTypeRepo.findById(taskTypeId);

    // Assert
    expect(foundTaskType).toEqual(taskType);
    expect(taskTypeSchemaMock.findOne).toBeCalledWith({ id: taskTypeId });
  });

  it('should find a task type by type', async () => {
    // Arrange
    const taskTypeType = Types.PickUpAndDelivery.toString();
    const taskType = TaskType.create({ type: Types.PickUpAndDelivery }).getValue();
    const taskTypePersistence = TaskTypeMap.toPersistence(taskType);

    taskTypeSchemaMock.findOne.mockResolvedValueOnce(taskTypePersistence as any);

    // Act
    const foundTaskType = await taskTypeRepo.findByType(taskTypeType);

    // Assert
    expect(foundTaskType).toEqual(taskType);
    expect(taskTypeSchemaMock.findOne).toBeCalledWith({ type: taskTypeType });
  });

  it('should check if a task type exists', async () => {
    // Arrange
    const taskTypeId = new UniqueEntityID();
    const taskType = TaskType.create({ type: Types.PickUpAndDelivery }, taskTypeId).getValue();
    const taskTypePersistence = TaskTypeMap.toPersistence(taskType);

    taskTypeSchemaMock.findOne.mockResolvedValueOnce(taskTypePersistence as any);

    // Act
    const exists = await taskTypeRepo.exists(taskType);

    // Assert
    expect(exists).toBe(true);
    expect(taskTypeSchemaMock.findOne).toBeCalledWith({ domainId: taskTypeId.toString() });
  });

  it('should fail to find a task type by ID if not found', async () => {
    // Arrange
    const taskTypeId = '00000000-0000-0000-0000-000000000001';

    taskTypeSchemaMock.findOne.mockResolvedValueOnce(null as any);

    // Act
    const foundTaskType = await taskTypeRepo.findById(taskTypeId);

    // Assert
    expect(foundTaskType).toBeNull();
    expect(taskTypeSchemaMock.findOne).toBeCalledWith({ id: taskTypeId });
  });

  it('should fail to find a task type by type if not found', async () => {
    // Arrange
    const taskTypeType = 'Pick';

    taskTypeSchemaMock.findOne.mockResolvedValueOnce(null as any);

    // Act
    const foundTaskType = await taskTypeRepo.findByType(taskTypeType);

    // Assert
    expect(foundTaskType).toBeNull();
    expect(taskTypeSchemaMock.findOne).toBeCalledWith({ type: taskTypeType });
  });
});
