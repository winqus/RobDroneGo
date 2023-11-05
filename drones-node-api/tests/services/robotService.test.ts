import { MockProxy, mock } from 'jest-mock-extended';
import { UniqueEntityID } from '../../src/core/domain/UniqueEntityID';
import { Result } from '../../src/core/logic/Result';
import { Robot } from '../../src/domain/Robot/robot';
import IRobotDTO from '../../src/dto/IRobotDTO';
import IRobotRepo from '../../src/services/IRepos/IRobotRepo';
import IRobotTypeRepo from '../../src/services/IRepos/IRobotTypeRepo';
import RobotService from '../../src/services/robotService';

describe('RobotService', () => {
  let robotService: RobotService;
  let robotRepoMock: MockProxy<IRobotRepo>;
  let robotTypeRepoMock: MockProxy<IRobotTypeRepo>;
  let robotStub: Robot;

  beforeEach(() => {
    robotRepoMock = mock<IRobotRepo>();
    robotTypeRepoMock = mock<IRobotTypeRepo>();

    robotStub = {
      id: new UniqueEntityID(),
      code: { value: 'A11' },
      description: { value: 'Sample' },
      nickname: { value: 'Nickname' },
      serialNumber: { value: 'A11' },
      available: true,
      type: { value: 'Type' },
    } as Robot;

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

  describe('changeRobotState', () => {
    it('should successfully change robot state', async () => {
      const robotDTO: Partial<IRobotDTO> = {
        code: 'A11',
        available: false,
      };

      robotRepoMock.findByCode.mockResolvedValue(robotStub as any);
      robotRepoMock.save.mockResolvedValue(robotStub as any);

      const result = await robotService.changeRobotState(robotDTO);

      expect(result.isSuccess).toBe(true);
      expect(robotRepoMock.save).toHaveBeenCalled();
    });

    it('should fail to change robot state if the current state is the same', async () => {
      const robotDTO: Partial<IRobotDTO> = {
        code: 'A11',
        available: true,
      };

      robotRepoMock.findByCode.mockResolvedValue(robotStub as any);
      robotRepoMock.save.mockResolvedValue(robotStub as any);

      const result = await robotService.changeRobotState(robotDTO);

      expect(result.isFailure).toBe(true);
      expect(robotRepoMock.save).not.toBeCalled();
    });
  });
});
