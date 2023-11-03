import { NextFunction, Request, Response } from 'express';

export default interface IRobotController {
  createRobot(req: Request, res: Response, next: NextFunction);

  changeRobotState(req: Request, res: Response, next: NextFunction);

  listAllRobots(req: Request, res: Response, next: NextFunction);

  getByType(req: Request, res: Response, next: NextFunction);
}
