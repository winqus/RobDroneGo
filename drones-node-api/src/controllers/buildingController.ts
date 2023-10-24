import { NextFunction, Request, Response } from 'express';
import { Inject, Service } from 'typedi';
import config from '../../config';
import { Result } from '../core/logic/Result';
import IBuildingDTO from '../dto/IBuildingDTO';
import IBuildingService from '../services/IServices/IBuildingService';
import IBuildingController from './IControllers/IBuildingController';

@Service()
export default class BuildingController implements IBuildingController {
  constructor(@Inject(config.services.building.name) private buildingService: IBuildingService) {}

  public async createBuilding(req: Request, res: Response, next: NextFunction) {
    try {
      const buildingOrError = (await this.buildingService.createBuilding(req.body as IBuildingDTO)) as Result<
        IBuildingDTO
      >;

      if (buildingOrError.isFailure) {
        return res.status(400).json({ message: buildingOrError.error.toString() });
      }

      const buildingDTO = buildingOrError.getValue();

      return res.status(201).json(buildingDTO);
    } catch (error) {
      return next(error);
    }
  }

  public async updateBuilding(req: Request, res: Response, next: NextFunction) {
    try {
      const buildingOrError = (await this.buildingService.updateBuilding({
        id: req.params.id,
        ...req.body,
      } as IBuildingDTO)) as Result<IBuildingDTO>;

      if (buildingOrError.isFailure) {
        return res.status(400).json({ message: buildingOrError.error.toString() });
      }

      const buildingDTO = buildingOrError.getValue();

      return res.status(200).json(buildingDTO);
    } catch (error) {
      return next(error);
    }
  }
}
