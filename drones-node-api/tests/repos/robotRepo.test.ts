import { Document, HydratedDocument, Model } from 'mongoose';
import { Container } from 'typedi';
import { UniqueEntityID } from '../../src/core/domain/UniqueEntityID';
import { Robot } from '../../src/domain/Robot/robot';
import RobotRepo from '../../src/repos/robotRepo';

describe('RobotRepo', () => {
  let robotRepo: RobotRepo;
  let robotSchemaMock: jest.Mocked<Model<Document>>;
  let robotStub: Robot;

  beforeEach(() => {
    robotSchemaMock = ({
      findOne: jest.fn(),
      create: jest.fn(),
    } as unknown) as jest.Mocked<Model<Document>>;
    Container.set('robotSchema', robotSchemaMock);

    robotRepo = new RobotRepo(robotSchemaMock as any);

    robotStub = {
      id: new UniqueEntityID(),
      code: { value: 'A11' },
      description: { value: 'Sample robot' },
      nickname: { value: 'Nickname' },
      serialNumber: { value: 'A11' },
      available: true,
      type: { value: 'Type' },
    } as Robot;
  });

  describe('exists', () => {
    it('should return true if robot exists', async () => {
      robotSchemaMock.findOne.mockResolvedValue({} as HydratedDocument<any, any, any>);

      const result = await robotRepo.exists(robotStub);

      expect(result).toBe(true);
    });

    it('should return false if robot does not exist', async () => {
      robotSchemaMock.findOne.mockResolvedValue(null as HydratedDocument<any, any, any>);

      const result = await robotRepo.exists(robotStub);

      expect(result).toBe(false);
    });
  });

  describe('save', () => {
    it('should save a new robot successfully', async () => {
      robotSchemaMock.findOne.mockResolvedValue(null as HydratedDocument<any, any, any>);
      robotSchemaMock.create.mockResolvedValue({
        id: robotStub.id.toString(),
        code: robotStub.code.value,
        description: robotStub.description.value,
        nickname: robotStub.nickname.value,
        serialNumber: robotStub.serialNumber.value,
        available: robotStub.available,
        type: robotStub.type.value,
      } as HydratedDocument<any, any, any>);

      await robotRepo.save(robotStub);

      expect(robotSchemaMock.create).toHaveBeenCalledTimes(1);
    });
  });
});
