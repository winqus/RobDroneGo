import { Result } from '../../src/core/logic/Result';
import TaskTypeService from '../../src/services/taskTypeService'; // Import your service class

// Mocked dependencies
const taskTypeRepoMock = {
  save: jest.fn(),
  findByType: jest.fn(),
  findById: jest.fn(),
  exists: jest.fn(),
};
const validTaskType = 'PickUpAndDelivery';
const invalidTaskType = 'InvalidType';

describe('TaskTypeService', () => {
  let taskTypeService: TaskTypeService;

  beforeEach(() => {
    taskTypeService = new TaskTypeService(taskTypeRepoMock);
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  it('should create a new task type successfully', async () => {
    // Arrange
    const taskTypeDTO = {
      id: '00000000-0000-0000-0000-000000000001',
      type: validTaskType,
    };

    taskTypeRepoMock.findByType.mockResolvedValue(null);

    // Act
    const result = await taskTypeService.createTaskType(taskTypeDTO);

    // Assert
    expect(result.isSuccess).toBe(true);
    expect(taskTypeRepoMock.save).toHaveBeenCalledTimes(1);
  });

  it('should fail to create a new task type if it already exists', async () => {
    // Arrange
    const taskTypeDTO = { id: '00000000-0000-0000-0000-000000000001', type: validTaskType };

    taskTypeRepoMock.findByType.mockResolvedValue({}); // Mock that the task type already exists

    // Act
    const result = await taskTypeService.createTaskType(taskTypeDTO);

    // Assert
    expect(result.isFailure).toBe(true);
    expect(result.error).toBe('Task Type already exists');
    expect(taskTypeRepoMock.save).not.toHaveBeenCalled();
  });

  it('should fail to create a new task type if it is invalid', async () => {
    // Arrange
    const taskTypeDTO = { id: '00000000-0000-0000-0000-000000000001', type: invalidTaskType };

    // Act
    const result = await taskTypeService.createTaskType(taskTypeDTO);

    // Assert
    expect(result.isFailure).toBe(true);
    expect(result.error).toBe('Invalid task type');
    expect(taskTypeRepoMock.save).not.toHaveBeenCalled();
  });

  it('should get an existing task type successfully', async () => {
    // Arrange
    const taskTypeDTO = { id: '00000000-0000-0000-0000-000000000001', type: validTaskType };
    const existingTaskType = { id: '00000000-0000-0000-0000-000000000001', type: validTaskType };

    taskTypeRepoMock.findByType.mockResolvedValue(existingTaskType);

    // Act
    const result = await taskTypeService.getTaskType(taskTypeDTO);

    // Assert
    expect(result.isSuccess).toBe(true);
  });

  it('should fail to get a task type if it does not exist', async () => {
    // Arrange
    const taskTypeDTO = { id: '00000000-0000-0000-0000-000000000001', type: invalidTaskType };

    taskTypeRepoMock.findByType.mockResolvedValue(null); // Mock that the task type does not exist

    // Act
    const result = await taskTypeService.getTaskType(taskTypeDTO);

    // Assert
    expect(result.isFailure).toBe(true);
    expect(result.error).toBe('TaskType not found');
  });
});
