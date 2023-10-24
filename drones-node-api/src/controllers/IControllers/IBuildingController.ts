import { NextFunction, Request, Response } from 'express';

export default interface IBuildingController {
  createBuilding(req: Request, res: Response, next: NextFunction);

  updateBuilding(req: Request, res: Response, next: NextFunction);

  // getBuilding(req: Request, res: Response, next: NextFunction);
}
