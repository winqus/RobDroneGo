import { NextFunction, Request, Response } from 'express';

export default interface IRobotTypeController {
  createRobotType(req: Request, res: Response, next: NextFunction);
}
