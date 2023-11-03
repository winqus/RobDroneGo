import { NextFunction, Request, Response } from 'express';
import { Result } from '../../core/logic/Result';
import IRobotTypeDTO from '../../dto/IRobotTypeDTO';
import IRobotTypeService from '../../services/IServices/IRobotTypeService';
import RobotTypeController from '../robotTypeController';

describe('RobotTypeController', () => {
  let robotTypeController: RobotTypeController;
  let robotTypeService: jest.Mocked<IRobotTypeService>;
  let req: Partial<Request>;
  let res: Partial<Response>;
  let next: NextFunction;

  beforeEach(() => {
    robotTypeService = {
      createRobotType: jest.fn(),
    };

    req = {
      body: {},
    };

    res = {
      status: jest.fn().mockReturnThis(),
      json: jest.fn().mockReturnThis(),
    };

    next = jest.fn();

    robotTypeController = new RobotTypeController(robotTypeService as IRobotTypeService);
  });

  describe('createRobotType', () => {
    it('should successfully create a robot type and return 201 status', async () => {
      const robotTypeDTO = {
        id: '00000000-0000-0000-0000-000000000000',
        name: 'Test Robot',
        brand: 'Test Brand',
        model: 'Test Model',
        typesOfTasks: ['TaskType1', 'TaskType2'],
      };

      robotTypeService.createRobotType.mockResolvedValue(Result.ok<IRobotTypeDTO>(robotTypeDTO) as any);

      await robotTypeController.createRobotType(req as Request, res as Response, next);

      expect(res.status).toHaveBeenCalledWith(201);
      expect(res.json).toHaveBeenCalledWith(robotTypeDTO);
    });

    it('should return 400 status if robot type creation fails', async () => {
      robotTypeService.createRobotType.mockResolvedValue(Result.fail<IRobotTypeDTO>('An error occurred') as any);

      await robotTypeController.createRobotType(req as Request, res as Response, next);

      expect(res.status).toHaveBeenCalledWith(400);
      expect(res.json).toHaveBeenCalledWith({ message: 'An error occurred' });
    });
  });
});
