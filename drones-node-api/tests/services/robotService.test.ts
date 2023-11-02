import { Container } from 'typedi';
import { UniqueEntityID } from '../../src/core/domain/UniqueEntityID';
import { Result } from '../../src/core/logic/Result';
import { Robot } from '../../src/domain/Robot/robot';
import IRobotDTO from '../../src/dto/IRobotDTO';
import IRobotRepo from '../../src/services/IRepos/IRobotRepo';
import IRobotTypeRepo from '../../src/services/IRepos/IRobotTypeRepo';
import RobotService from '../../src/services/robotService';

describe('RobotService', () => {
  let robotService: RobotService;
  let robotRepoMock: jest.Mocked<IRobotRepo>;
  let robotTypeRepoMock: jest.Mocked<IRobotTypeRepo>;
  let robotStub: Robot;

  beforeEach(() => {
    robotRepoMock = {
      save: jest.fn(),
      exists: jest.fn(),
    };

    robotTypeRepoMock = {
      findById: jest.fn(),
      findByBrandAndModel: jest.fn(),
      save: jest.fn(),
      exists: jest.fn(),
      findByName: jest.fn(),
    };

    robotStub = {
      id: new UniqueEntityID(),
      code: { value: 'A11' },
      description: { value: 'Sample' },
      nickname: { value: 'Nickname' },
      serialNumber: { value: 'A11' },
      available: true,
      type: { value: 'Type' },
    } as Robot;

    Container.set('robotRepo', robotRepoMock);
    Container.set('robotTypeRepo', robotTypeRepoMock);
    robotService = new RobotService(robotRepoMock, robotTypeRepoMock);
  });

  describe('createRobot', () => {
    it('should successfully create a robot', async () => {
      const robotDTO: IRobotDTO = {
        id: '00000000-0000-0000-0000-000000000000',
        code: 'A11',
        description: 'Sample robot',
        nickname: 'Nickname',
        serialNumber: 'A11',
        available: true,
        type: 'Type',
      };

      robotRepoMock.save.mockResolvedValue(robotStub as any);
      robotTypeRepoMock.findByName.mockResolvedValue(Result.ok(true) as any);

      const result = await robotService.createRobot(robotDTO);

      expect(result.isSuccess).toBe(true);
      expect(robotRepoMock.save).toBeCalled();
    });

    it('should fail to create a robot when input is invalid', async () => {
      const robotDTO: IRobotDTO = {
        id: '00000000-0000-0000-0000-000000000000',
        code: 'A11',
        description: 'Sample robot',
        nickname: 'Nickname'.repeat(10),
        serialNumber: 'A11',
        available: true,
        type: 'Type',
      };

      const result = await robotService.createRobot(robotDTO);

      expect(result.isFailure).toBe(true);
      expect(robotRepoMock.save).not.toBeCalled();
    });

    it('should fail to create a robot if it already exists', async () => {
      const robotDTO: IRobotDTO = {
        id: '00000000-0000-0000-0000-000000000000',
        code: 'A11',
        description: 'Sample robot',
        nickname: 'Nickname',
        serialNumber: 'A11',
        available: true,
        type: 'Type',
      };

      robotRepoMock.save.mockResolvedValue(robotStub as any);
      robotRepoMock.exists.mockResolvedValue(Result.fail('Robot already exists') as any);

      const result = await robotService.createRobot(robotDTO);

      expect(result.isFailure).toBe(true);
      expect(robotRepoMock.save).not.toBeCalled();
    });
  });
});
