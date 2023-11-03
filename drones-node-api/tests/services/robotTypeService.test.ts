import { Container } from 'typedi';
import { Result } from '../../src/core/logic/Result';
import { TaskType } from '../../src/domain/TaskType/taskType';
import { Types } from '../../src/domain/TaskType/type';
import RobotTypeService from '../../src/services/robotTypeService'; // Import your service class

const robotTypeRepoMock = {
  save: jest.fn(),
  findByName: jest.fn(),
  findById: jest.fn(),
  findByBrandAndModel: jest.fn(),
  exists: jest.fn(),
};
const taskTypeServiceMock = {
  getTaskType: jest.fn(),
  createTaskType: jest.fn(),
};

describe('RobotTypeService', () => {
  let robotTypeService: RobotTypeService;

  beforeEach(() => {
    // Mocked dependencies
    robotTypeService = new RobotTypeService(robotTypeRepoMock, taskTypeServiceMock);
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  it('should create a new robot type successfully', async () => {
    // Arrange
    const robotTypeDTO = {
      id: '00000000-0000-0000-0000-000000000001',
      name: 'TestRobot',
      brand: 'TestBrand',
      model: 'TestModel',
      typesOfTasks: ['PickUpAndDelivery'],
    };

    robotTypeRepoMock.findByName.mockResolvedValue(null);
    taskTypeServiceMock.getTaskType.mockResolvedValue(Result.ok({ type: Types.Surveillance }));

    // Act
    const result = await robotTypeService.createRobotType(robotTypeDTO);

    // Assert
    expect(result.isSuccess).toBe(true);
    expect(robotTypeRepoMock.save).toHaveBeenCalledTimes(1);
  });

  it('should fail to create a new robot type if it already exists', async () => {
    // Arrange
    const robotTypeDTO = {
      id: '00000000-0000-0000-0000-000000000001',
      name: 'TestRobot',
      brand: 'TestBrand',
      model: 'TestModel',
      typesOfTasks: ['PickUpAndDelivery'],
    };

    robotTypeRepoMock.findByName.mockResolvedValue({}); // Mock that the robot type already exists

    // Act
    const result = await robotTypeService.createRobotType(robotTypeDTO);

    // Assert
    expect(result.isFailure).toBe(true);
    expect(result.error).toBe('Robot Type already exists');
    expect(robotTypeRepoMock.save).not.toHaveBeenCalled();
  });

  it('should fail to create a new robot type if there are validation errors', async () => {
    // Arrange
    const robotTypeDTO = {
      id: '00000000-0000-0000-0000-000000000001',
      name: 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', // Invalid name
      brand: 'TestBrand',
      model: 'TestModel',
      typesOfTasks: ['PickUpAndDelivery'],
    };
    taskTypeServiceMock.getTaskType.mockResolvedValue(Result.ok({ type: Types.Surveillance }));

    // Act
    const result = await robotTypeService.createRobotType(robotTypeDTO);

    // Assert
    expect(result.isFailure).toBe(true);
    expect(result.error).toContain('name length should be between 0 and 50 characters');
    expect(robotTypeRepoMock.save).not.toHaveBeenCalled();
  });

  it('should fail to create a new robot type if task types cannot be retrieved', async () => {
    // Arrange
    const robotTypeDTO = {
      id: '00000000-0000-0000-0000-000000000001',
      name: 'TestRobot',
      brand: 'TestBrand',
      model: 'TestModel',
      typesOfTasks: ['Task1'],
    };

    taskTypeServiceMock.getTaskType.mockResolvedValue(Result.fail('Task Type not found'));
    taskTypeServiceMock.createTaskType.mockResolvedValue(Result.fail('Task Type not found'));

    // Act
    const result = await robotTypeService.createRobotType(robotTypeDTO);

    // Assert
    expect(result.isFailure).toBe(true);
    expect(result.error).toBe('Task Type not found');
    expect(robotTypeRepoMock.save).not.toHaveBeenCalled();
  });
});
