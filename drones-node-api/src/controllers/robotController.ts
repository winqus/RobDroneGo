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
}
