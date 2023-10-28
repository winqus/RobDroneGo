import { NextFunction, Request, Response } from 'express';
import { Inject, Service } from 'typedi';
import config from '../../config';
import { Result } from '../core/logic/Result';
import IFloorDTO from '../dto/IFloorDTO';
import IFloorService from '../services/IServices/IFloorService';
import IFloorController from './IControllers/IFloorController';

@Service()
export default class FloorController implements IFloorController {
  constructor(@Inject(config.services.floor.name) private floorService: IFloorService) {}

  public async createFloor(req: Request, res: Response, next: NextFunction) {
    try {
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
