import { NextFunction, Request, Response } from 'express';
import { Inject, Service } from 'typedi';
import config from '../../config';
import { Result } from '../core/logic/Result';
import { Building } from '../domain/Building/building';
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

  public async listAllBuildings(req: Request, res: Response, next: NextFunction) {
    try {
      const buildingsResult = await this.buildingService.getAllBuildings();

      if (buildingsResult.isFailure) {
        return res.status(500).json({ message: buildingsResult.error.toString() });
      }

      const buildingDTOs: IBuildingDTO[] = buildingsResult.getValue();

      return res.status(200).json(buildingDTOs);
    } catch (error) {
      return next(error);
    }
  }

  public async getBuildingByCode(req: Request, res: Response, next: NextFunction) {
    try {
      const buildingCode: string = req.params.code;

      const buildingResult = await this.buildingService.getBuildingByCode(buildingCode);

      if (buildingResult.isFailure) {
        return res.status(404).json({ message: buildingResult.error.toString() });
      }

      const buildingDTO: IBuildingDTO = buildingResult.getValue();

      return res.status(200).json(buildingDTO);
    } catch (error) {
      return next(error);
    }
  }
}
