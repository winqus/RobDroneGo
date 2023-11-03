import { NextFunction, Request, Response } from 'express';

export default interface IRobotController {
  createRobot(req: Request, res: Response, next: NextFunction);
  changeRobotState(req: Request, res: Response, next: NextFunction);
}
