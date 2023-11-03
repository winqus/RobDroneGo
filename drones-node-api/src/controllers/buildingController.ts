import { NextFunction, Request, Response } from 'express';
import { Inject, Service } from 'typedi';
import config from '../../config';
import { Result } from '../core/logic/Result';
import { Building } from '../domain/Building/building';
import IBuildingDTO from '../dto/IBuildingDTO';
import IElevatorDTO from '../dto/IElevatorDTO';
import IBuildingService from '../services/IServices/IBuildingService';
import IElevatorService from '../services/IServices/IElevadorService';
import IBuildingController from './IControllers/IBuildingController';

@Service()
export default class BuildingController implements IBuildingController {
  constructor(
    @Inject(config.services.building.name) private buildingService: IBuildingService,
    @Inject(config.services.elevator.name) private elevatorService: IElevatorService,
  ) {}

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
        return res.status(404).json({ message: buildingsResult.error.toString() });
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

  public async getBuildingByFloorRange(req: Request, res: Response, next: NextFunction) {
    const minFloor: number = parseInt(req.query.minFloor as string, 10);
    const maxFloor: number = parseInt(req.query.maxFloor as string, 10);

    try {
      const buildingsResult = await this.buildingService.getBuildingsByFloorRange(minFloor, maxFloor);

      if (buildingsResult.isFailure) {
        return res.status(404).json({ message: buildingsResult.error.toString() });
      }

      const buildingDTOs: IBuildingDTO[] = buildingsResult.getValue();

      return res.status(200).json(buildingDTOs);
    } catch (error) {
      return next(error);
    }
  }

  public async createElevator(req: Request, res: Response, next: NextFunction) {
    try {
      const elevatorOrError = (await this.elevatorService.createElevator(
        req.body as IElevatorDTO,
        req.params.code,
        req.body.floors,
      )) as Result<IBuildingDTO>;

      if (elevatorOrError.isFailure) {
        return res.status(400).json({ message: elevatorOrError.error.toString() });
      }

      const elevatorDTO = elevatorOrError.getValue();

      return res.status(201).json(elevatorDTO);
    } catch (error) {
      return next(error);
    }
  }

  public async updateElevator(req: Request, res: Response, next: NextFunction) {
    try {
      const elevatorOrError = (await this.elevatorService.updateElevator(
        req.body as IElevatorDTO,
        req.params.code,
      )) as Result<IBuildingDTO>;

      if (elevatorOrError.isFailure) {
        return res.status(400).json({ message: elevatorOrError.error.toString() });
      }

      const elevatorDTO = elevatorOrError.getValue();

      return res.status(200).json(elevatorDTO);
    } catch (error) {
      return next(error);
    }
  }
}
