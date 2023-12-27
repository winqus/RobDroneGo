import { NextFunction, Request, Response } from 'express';
import { ParsedQs } from 'qs';
import { Inject, Service } from 'typedi';
import config from '../../config';
import { Result } from '../core/logic/Result';
import IPlanningService from '../services/IServices/IPlanningService';
import IPlanningController from './IControllers/IPlanningController';

@Service()
export default class PlanningController implements IPlanningController {
  constructor(@Inject(config.services.planning.name) private planningService: IPlanningService) {}

  public async calculateCells(req: Request, res: Response, next: NextFunction) {
    try {
      const query: ParsedQs = req.query;

      const result: Result<any[]> = await this.planningService.calculateCells(query);

      if (result.isFailure) {
        return res.status(400).json({ message: result.error.toString() });
      }

      const cells = result.getValue();

      return cells;
    } catch (error) {
      return next(error);
    }
  }

  public async planTasks(req: Request, res: Response, next: NextFunction) {
    try {
      const result: Result<any> = await this.planningService.planTasks(req.body.taskRequestIds);

      if (result.isFailure) {
        return res.status(400).json({ message: result.error.toString() });
      }

      return res.status(201).json(result.getValue());
    } catch (error) {
      return next(error);
    }
  }
}
