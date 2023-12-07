import { NextFunction, Request, Response } from 'express';

export default interface ITaskController {
  create(req: Request, res: Response, next: NextFunction);

  update(req: Request, res: Response, next: NextFunction);

  getAll(req: Request, res: Response, next: NextFunction);

  getById(req: Request, res: Response, next: NextFunction);
}
