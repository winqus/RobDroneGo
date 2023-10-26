import { NextFunction, Request, Response } from 'express';
import { Inject, Service } from 'typedi';
import config from '../../config';
import { Result } from '../core/logic/Result';
import IFloorDTO from '../dto/IFloorDTO';
import IBuildingService from '../services/IServices/IBuildingService';
import IFloorService from '../services/IServices/IFloorService';
import IFloorController from './IControllers/IFloorController';

@Service()
export default class FloorController implements IFloorController {
  constructor(
    @Inject(config.services.floor.name) private floorService: IFloorService,
    @Inject(config.services.building.name) private buildingService: IBuildingService,
  ) {}

  public async createFloor(req: Request, res: Response, next: NextFunction) {
    try {
      const buildingResult = await this.buildingService.getBuildingByCode(req.body.buildingCode);

      if (buildingResult.isFailure) {
        return res.status(400).json({ message: 'Building with the provided code does not exist.' });
      }

      const floorOrError = (await this.floorService.createFloor(req.body as IFloorDTO)) as Result<IFloorDTO>;

      if (floorOrError.isFailure) {
        return res.status(400).json({ message: floorOrError.error.toString() });
      }

      const floorDTO = floorOrError.getValue();

      return res.status(201).json(floorDTO);
    } catch (error) {
      return next(error);
    }
  }
}
