import { NextFunction, Request, Response } from 'express';

export default interface IBuildingController {
  createBuilding(req: Request, res: Response, next: NextFunction);

  updateBuilding(req: Request, res: Response, next: NextFunction);

  listAllBuildings(req: Request, res: Response, next: NextFunction);

  getBuildingByCode(req: Request, res: Response, next: NextFunction);

  getBuildingByFloorRange(req: Request, res: Response, next: NextFunction);

  createElevator(req: Request, res: Response, next: NextFunction);

  updateElevator(req: Request, res: Response, next: NextFunction);

  listElevatorsInBuilding(req: Request, res: Response, next: NextFunction);
}
