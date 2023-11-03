import { Document, Model } from 'mongoose';
import { Brand } from '../../src/domain/RobotType/ValueObjects/brand';
import { Model as ModelValue } from '../../src/domain/RobotType/ValueObjects/model';
import { Name } from '../../src/domain/RobotType/ValueObjects/name';
import { RobotType } from '../../src/domain/RobotType/robotType';
import { TaskType } from '../../src/domain/TaskType/taskType';
import { Types } from '../../src/domain/TaskType/type';
import RobotTypeRepo from '../../src/repos/robotTypeRepo';

describe('RobotTypeRepo', () => {
  let robotTypeRepo: RobotTypeRepo;
  let robotTypeSchemaMock: jest.Mocked<Model<Document>>;
  let robotType: RobotType;

  beforeEach(() => {
    robotTypeSchemaMock = ({
      findOne: jest.fn(),
      create: jest.fn(),
    } as unknown) as jest.Mocked<Model<Document>>;
    robotTypeRepo = new RobotTypeRepo(robotTypeSchemaMock as any);

    robotType = RobotType.create({
      name: Name.create('test').getValue(),
      brand: Brand.create('test').getValue(),
      model: ModelValue.create('test').getValue(),
      typesOfTasks: [TaskType.create({ type: Types.Surveillance }).getValue()],
    }).getValue();
  });

  describe('exists', () => {
    it('should return true if robot type exists', async () => {
      // Arrange
      robotTypeSchemaMock.findOne.mockResolvedValue({} as any);

      // Act
      const result = await robotTypeRepo.exists(robotType);

      // Assert
      expect(result).toBe(true);
    });

    it('should return false if robot type does not exist', async () => {
      // Arrange
      robotTypeSchemaMock.findOne.mockResolvedValue(null as any);

      // Act
      const result = await robotTypeRepo.exists(robotType);

      // Assert
      expect(result).toBe(false);
    });
  });

  describe('save', () => {
    it('should save a new robot type successfully', async () => {
      // Arrange
      robotTypeSchemaMock.findOne.mockResolvedValue(null as any);
      robotTypeSchemaMock.create.mockResolvedValue({
        id: robotType.id.toString(),
        name: robotType.name.value,
        brand: robotType.brand.value,
        model: robotType.model.value,
        typesOfTasks: robotType.typesOfTasks.map((taskType) => ({
          type: taskType.type,
        })),
      } as any);

      // Act
      const savedRobotType = await robotTypeRepo.save(robotType);

      // Assert
      expect(robotTypeSchemaMock.create).toHaveBeenCalledTimes(1);
      // Add further assertions for savedRobotType if needed
    });

    it('should update an existing robot type successfully', async () => {
      // Arrange
      const saveStub = jest.fn();
      robotTypeSchemaMock.findOne.mockResolvedValue({ save: saveStub } as any);

      // Act
      const savedRobotType = await robotTypeRepo.save(robotType);

      // Assert
      expect(saveStub).toHaveBeenCalledTimes(1);
      // Add further assertions for savedRobotType if needed
    });
  });

  // Add more test cases for findById, findByName, and findByBrandAndModel as needed
});
