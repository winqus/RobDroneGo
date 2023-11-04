import { NextFunction, Request, Response } from 'express';

export default interface IPassageController {
  createPassage(req: Request, res: Response, next: NextFunction);

  getPassages(req: Request, res: Response, next: NextFunction);

  listFloorsWithPassagesToDifferentBuilding(req: Request, res: Response, next: NextFunction);

  updatePassage(req: Request, res: Response, next: NextFunction);
}
