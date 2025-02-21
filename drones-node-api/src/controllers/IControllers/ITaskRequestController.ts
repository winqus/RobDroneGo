import { NextFunction, Request, Response } from 'express';

export default interface ITaskController {
  create(req: Request, res: Response, next: NextFunction);

  changeState(req: Request, res: Response, next: NextFunction);

  addNavigationData(req: Request, res: Response, next: NextFunction);

  getAll(req: Request, res: Response, next: NextFunction);

  getById(req: Request, res: Response, next: NextFunction);
}
