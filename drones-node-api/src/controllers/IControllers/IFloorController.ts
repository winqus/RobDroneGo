import { NextFunction, Request, Response } from 'express';

export default interface IFloorController {
  updateFloor(req: Request, res: Response, next: NextFunction);
  partialUpdateFloor(req: Request, res: Response, next: NextFunction);
  createFloor(req: Request, res: Response, next: NextFunction);
}
