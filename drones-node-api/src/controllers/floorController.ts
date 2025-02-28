import { NextFunction, Request, Response } from 'express';
import { ParamsDictionary } from 'express-serve-static-core';
import { MongoError } from 'mongodb';
import { ParsedQs } from 'qs';
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
      if (error instanceof MongoError && error.code === 11000) {
        return res
          .status(400)
          .json({ message: 'Some unique fields were duplicated. More information: ' + error.message });
      }

      return next(error);
    }
  }

  public async updateFloor(req: Request, res: Response, next: NextFunction) {
    try {
      const floorId: string = req.params.floorId;

      if (!floorId) {
        return res.status(400).json({ message: 'Floor ID is required' });
      }

      const updatedFloorDTO: IFloorDTO = req.body;
      updatedFloorDTO.id = floorId;

      const result: Result<IFloorDTO> = await this.floorService.updateFloor(updatedFloorDTO);

      if (result.isFailure) {
        return res.status(400).json({ message: result.error.toString() });
      }

      const floorDTO = result.getValue();

      return res.status(200).json(floorDTO);
    } catch (error) {
      return next(error);
    }
  }

  public async partialUpdateFloor(req: Request, res: Response, next: NextFunction) {
    try {
      const floorId: string = req.params.floorId;

      if (!floorId) {
        return res.status(400).json({ message: 'Floor ID is required' });
      }
      const updatedFloorDTO: IFloorDTO = req.body;
      updatedFloorDTO.id = floorId;

      const result: Result<IFloorDTO> = await this.floorService.partialUpdateFloor(updatedFloorDTO);

      if (result.isFailure) {
        return res.status(400).json({ message: result.error.toString() });
      }

      const floorDTO = result.getValue();

      return res.status(200).json(floorDTO);
    } catch (error) {
      return next(error);
    }
  }

  public async listAllFloors(req: Request, res: Response, next: NextFunction) {
    try {
      const floorsResult = await this.floorService.getAllFloors();

      if (floorsResult.isFailure) {
        return res.status(404).json({ message: floorsResult.error.toString() });
      }

      const floorDTOs: IFloorDTO[] = floorsResult.getValue();

      return res.status(200).json(floorDTOs);
    } catch (error) {
      return next(error);
    }
  }

  public async getFloorsByBuildingCode(req: Request, res: Response, next: NextFunction) {
    try {
      const codeBuilding: string = req.params.buildingCode;

      const floorResult = await this.floorService.getFloorsByBuildingCode(codeBuilding);

      if (floorResult.isFailure) {
        return res.status(404).json({ message: floorResult.error.toString() });
      }

      const floorDTOs: IFloorDTO[] = floorResult.getValue();

      return res.status(200).json(floorDTOs);
    } catch (error) {
      return next(error);
    }
  }

  public async getFloorsServedByElevator(req: Request, res: Response, next: NextFunction) {
    try {
      const buildingCode: string = req.query.buildingCode as string;

      const floorResult = await this.floorService.getFloorsServedByElevator(buildingCode);

      if (floorResult.isFailure) {
        return res.status(404).json({ message: floorResult.error.toString() });
      }

      const floorDTOs: IFloorDTO[] = floorResult.getValue();

      return res.status(200).json(floorDTOs);
    } catch (error) {
      return next(error);
    }
  }

  public async loadMap(req: Request, res: Response, next: NextFunction) {
    try {
      const buildingCode: string = req.params.buildingCode;
      const floorNumber: number = Number(req.params.floorNumber);
      const map = req.body.map;

      const floorResult = await this.floorService.loadMap(buildingCode, floorNumber, map);

      if (floorResult.isFailure) {
        return res.status(404).json({ message: floorResult.error.toString() });
      }

      const floorDTO: IFloorDTO = floorResult.getValue();

      return res.status(200).json(floorDTO);
    } catch (error) {
      return next(error);
    }
  }

  public async getMap(req: Request, res: Response, next: NextFunction) {
    try {
      const buildingCode: string = req.params.buildingCode;
      const floorNumber: number = Number(req.params.floorNumber);

      const mapResult = await this.floorService.getMap(buildingCode, floorNumber);

      if (mapResult.isFailure) {
        return res.status(404).json({ message: mapResult.error.toString() });
      }

      const mapDTO = mapResult.getValue();

      return res.status(200).json(mapDTO);
    } catch (error) {
      return next(error);
    }
  }
}
