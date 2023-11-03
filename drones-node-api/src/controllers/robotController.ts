import { NextFunction, Request, Response } from 'express';
import { MongoError } from 'mongodb';
import { Inject, Service } from 'typedi';
import config from '../../config';
import { Result } from '../core/logic/Result';
import IRobotDTO from '../dto/IRobotDTO';
import IRobotService from '../services/IServices/IRobotService';
import IRobotController from './IControllers/IRobotController';

@Service()
export default class RobotController implements IRobotController {
  constructor(@Inject(config.services.robot.name) private robotService: IRobotService) {}

  public async createRobot(req: Request, res: Response, next: NextFunction) {
    try {
      const robotOrError = (await this.robotService.createRobot(req.body as IRobotDTO)) as Result<IRobotDTO>;

      if (robotOrError.isFailure) {
        return res.status(400).json({ message: robotOrError.error.toString() });
      }

      const robotDTO = robotOrError.getValue();

      return res.status(201).json(robotDTO);
    } catch (error) {
      if (error instanceof MongoError && error.code === 11000) {
        return res
          .status(400)
          .json({ message: 'Some unique fields were duplicated. More information: ' + error.message });
      }

      return next(error);
    }
  }

  public async changeRobotState(req: Request, res: Response, next: NextFunction) {
    try {
      const robotCode: string = req.params.robotCode;

      if (!robotCode) {
        return res.status(400).json({ message: 'Robot code is required' });
      }

      const updatedRobotDTO: IRobotDTO = req.body;
      updatedRobotDTO.code = robotCode;

      const result: Result<IRobotDTO> = await this.robotService.changeRobotState(updatedRobotDTO);

      if (result.isFailure) {
        return res.status(400).json({ message: result.error.toString() });
      }

      const robotDTO = result.getValue();

      return res.status(200).json(robotDTO);
    } catch (error) {
      return next(error);
    }
  }

  public async listAllRobots(req: Request, res: Response, next: NextFunction) {
    try {
      const robotsResult = await this.robotService.getAllRobots();

      if (robotsResult.isFailure) {
        return res.status(404).json({ message: robotsResult.error.toString() });
      }

      const robotsDTOs: IRobotDTO[] = robotsResult.getValue();

      return res.status(200).json(robotsDTOs);
    } catch (error) {
      return next(error);
    }
  }
}
