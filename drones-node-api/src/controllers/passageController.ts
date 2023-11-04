import { NextFunction, Request, Response } from 'express';
import { ParsedQs } from 'qs';
import { Inject, Service } from 'typedi';
import config from '../../config';
import { Result } from '../core/logic/Result';
import IPassageDTO from '../dto/IPassageDTO';
import IPassageService from '../services/IServices/IPassageService';
import IPassageController from './IControllers/IPassageController';

@Service()
export default class PassageController implements IPassageController {
  constructor(@Inject(config.services.passage.name) private passageService: IPassageService) {}

  public async createPassage(req: Request, res: Response, next: NextFunction) {
    try {
      const passageOrError = (await this.passageService.createPassage(req.body as IPassageDTO)) as Result<IPassageDTO>;

      if (passageOrError.isFailure) {
        return res.status(400).json({ message: passageOrError.error.toString() });
      }

      const passageDTO = passageOrError.getValue();

      return res.status(201).json(passageDTO);
    } catch (error) {
      return next(error);
    }
  }

  public async getPassages(req: Request, res: Response, next: NextFunction) {
    try {
      const { buildingCode1, buildingCode2 } = req.query as ParsedQs;

      let passagesResult: Result<IPassageDTO[]>;

      if (typeof buildingCode1 === 'string' && typeof buildingCode2 === 'string') {
        passagesResult = await this.passageService.getPassagesBetweenBuildings(buildingCode1, buildingCode2);
      } else {
        passagesResult = await this.passageService.getAllPassages();
      }

      if (passagesResult.isFailure) {
        return res.status(400).json({ message: passagesResult.error.toString() });
      }

      const passagesDTO = passagesResult.getValue();

      return res.status(200).json(passagesDTO);
    } catch (error) {
      return next(error);
    }
  }

  public async listFloorsWithPassagesToDifferentBuilding(req: Request, res: Response, next: NextFunction) {
    try {
      const buildingCode = req.query.buildingCode as string;

      const listFloorsResult = await this.passageService.listFloorsWithPassagesToDifferentBuilding(buildingCode);

      if (listFloorsResult.isFailure) {
        return res.status(400).json({ message: listFloorsResult.error.toString() });
      }

      const listFloorsDTO = listFloorsResult.getValue();

      return res.status(200).json(listFloorsDTO);
    } catch (error) {
      return next(error);
    }
  }
}
