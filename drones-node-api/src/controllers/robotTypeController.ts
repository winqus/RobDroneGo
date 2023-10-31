import { NextFunction, Request, Response } from 'express';
import { Inject, Service } from 'typedi';
import config from '../../config';
import { Result } from '../core/logic/Result';
import IRobotTypeDTO from '../dto/IRobotTypeDTO';
import IRobotTypeService from '../services/IServices/IRobotTypeService';
import IRobotTypeController from './IControllers/IRobotTypeController';

@Service()
export default class RobotTypeController implements IRobotTypeController {
  constructor(@Inject(config.services.robotType.name) private robotTypeService: IRobotTypeService) {}

  public async createRobotType(req: Request, res: Response, next: NextFunction) {
    try {
      const robotTypeOrError = (await this.robotTypeService.createRobotType(req.body as IRobotTypeDTO)) as Result<
        IRobotTypeDTO
      >;

      if (robotTypeOrError.isFailure) {
        return res.status(400).json({ message: robotTypeOrError.error.toString() });
      }

      const robotTypeDTO = robotTypeOrError.getValue();

      return res.status(201).json(robotTypeDTO);
    } catch (error) {
      return next(error);
    }
  }
}
