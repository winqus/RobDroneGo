import { NextFunction, Request, Response } from 'express';
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
}
