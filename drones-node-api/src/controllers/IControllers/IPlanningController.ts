import { NextFunction, Request, Response } from 'express';

export default interface IPlanningController {
  calculateCells(req: Request, res: Response, next: NextFunction);
  planTasks(req: Request, res: Response, next: NextFunction);
}
