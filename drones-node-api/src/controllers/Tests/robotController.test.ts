import { NextFunction, Request, Response } from 'express';
import { Container } from 'typedi';
import { Result } from '../../core/logic/Result';
import IRobotDTO from '../../dto/IRobotDTO';
import IRobotService from '../../services/IServices/IRobotService';
import RobotController from '../robotController';

describe('RobotController', () => {
  let robotController: RobotController;
  let robotServiceMock: jest.Mocked<IRobotService>;
  let reqMock: Partial<Request>;
  let resMock: Partial<Response>;
  let nextMock: NextFunction;

  beforeEach(() => {
    robotServiceMock = {
      createRobot: jest.fn(),
      changeRobotState: jest.fn(),
    } as jest.Mocked<IRobotService>;

    reqMock = {
      body: {},
    };

    resMock = {
      status: jest.fn().mockReturnThis(),
      json: jest.fn().mockReturnThis(),
    };

    nextMock = jest.fn();

    Container.set('robotService', robotServiceMock);
    robotController = new RobotController(robotServiceMock);
  });

  describe('createRobot', () => {
    it('should successfully create a robot and return 201 status', async () => {
      const robotDTO: IRobotDTO = {
        id: '00000000-0000-0000-0000-000000000000',
        code: 'A11',
        description: 'Sample robot',
        nickname: 'Nickname',
        serialNumber: 'A11',
        available: true,
        type: 'Type',
      };

      robotServiceMock.createRobot.mockResolvedValue(Result.ok<IRobotDTO>(robotDTO) as any);

      await robotController.createRobot(reqMock as Request, resMock as Response, nextMock);

      expect(resMock.status).toHaveBeenCalledWith(201);
      expect(resMock.json).toHaveBeenCalledWith(robotDTO);
    });

    it('should return 400 status if robot creation fails', async () => {
      robotServiceMock.createRobot.mockResolvedValue(Result.fail<IRobotDTO>('Error') as any);

      await robotController.createRobot(reqMock as Request, resMock as Response, nextMock);

      expect(resMock.status).toHaveBeenCalledWith(400);
      expect(resMock.json).toHaveBeenCalledWith({ message: 'Error' });
    });
  });

  describe('changeRobotState', () => {
    it('should successfully change robot state and return 200 status', async () => {
      const robotDTO: IRobotDTO = {
        id: '00000000-0000-0000-0000-000000000000',
        code: 'A11',
        description: 'Sample robot',
        nickname: 'Nickname',
        serialNumber: 'A11',
        available: true,
        type: 'Type',
      };

      reqMock.params = { robotCode: robotDTO.code };
      reqMock.body = { available: false };

      robotServiceMock.changeRobotState.mockResolvedValue(Result.ok<IRobotDTO>(robotDTO) as any);

      await robotController.changeRobotState(reqMock as Request, resMock as Response, nextMock);

      expect(resMock.status).toHaveBeenCalledWith(200);
      expect(resMock.json).toHaveBeenCalledWith(robotDTO);
    });

    it('should fail to change robot state and return 400 status when requested state is already applyed', async () => {
      const robotDTO: IRobotDTO = {
        id: '00000000-0000-0000-0000-000000000000',
        code: 'A11',
        description: 'Sample robot',
        nickname: 'Nickname',
        serialNumber: 'A11',
        available: true,
        type: 'Type',
      };

      reqMock.params = { robotCode: robotDTO.code };
      reqMock.body = { available: true };

      robotServiceMock.changeRobotState.mockResolvedValue(Result.fail<IRobotDTO>('Error changing the state') as any);

      await robotController.changeRobotState(reqMock as Request, resMock as Response, nextMock);

      expect(resMock.status).toHaveBeenCalledWith(400);
      expect(resMock.json).toHaveBeenCalledWith({ message: 'Error changing the state' });
    });
  });
});
