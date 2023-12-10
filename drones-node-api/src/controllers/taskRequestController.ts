import { NextFunction, Request, Response } from 'express';
import { Inject, Service } from 'typedi';
import config from '../../config';
import { Result } from '../core/logic/Result';
import { ITaskRequestDTO } from '../dto/ITaskRequestDTO';
import ITaskRequestService from '../services/IServices/ITaskRequestService';
import ITaskRequestController from './IControllers/ITaskRequestController';

@Service()
export default class TaskRequestController implements ITaskRequestController {
  constructor(@Inject(config.services.taskRequest.name) private taskRequestService: ITaskRequestService) {}

  public async create(req: Request, res: Response, next: NextFunction) {
    try {
      const taskRequestOrError = await this.taskRequestService.create(req.body as ITaskRequestDTO);

      if (taskRequestOrError.isFailure) {
        return res.status(400).json({ message: taskRequestOrError.error.toString() });
      }

      const taskRequestDTO = taskRequestOrError.getValue();

      return res.status(201).json(taskRequestDTO);
    } catch (error) {
      return next(error);
    }
  }

  public async changeState(req: Request, res: Response, next: NextFunction) {
    try {
      const taskRequestID: string = req.params.id;

      if (!taskRequestID) {
        return res.status(400).json({ message: 'Task request ID is required' });
      }

      const updatedTaskRequestDTO: ITaskRequestDTO = req.body;
      updatedTaskRequestDTO.id = taskRequestID;

      const result: Result<ITaskRequestDTO> = await this.taskRequestService.changeState(updatedTaskRequestDTO);

      if (result.isFailure) {
        return res.status(400).json({ message: result.error.toString() });
      }

      const taskRequestDTO = result.getValue();

      return res.status(200).json(taskRequestDTO);
    } catch (error) {
      return next(error);
    }
  }

  public async addNavigationData(req: Request, res: Response, next: NextFunction) {
    try {
      const taskRequestID: string = req.params.id;
      console.log(req.body);

      const navigationData: INavigationDataDTO = req.body;

      const taskRequestResult = await this.taskRequestService.addNavigationData(taskRequestID, navigationData);

      if (taskRequestResult.isFailure) {
        return res.status(400).json({ message: taskRequestResult.error.toString() });
      }

      const taskRequestDTO = taskRequestResult.getValue();

      return res.status(200).json(taskRequestDTO);
    } catch (error) {
      return next(error);
    }
  }

  public async getAll(req: Request, res: Response, next: NextFunction) {
    try {
      const taskRequestsResult = await this.taskRequestService.getAll();

      if (taskRequestsResult.isFailure) {
        return res.status(400).json({ message: taskRequestsResult.error.toString() });
      }

      const taskRequestsDTOs: ITaskRequestDTO[] = taskRequestsResult.getValue();

      return res.status(200).json(taskRequestsDTOs);
    } catch (error) {
      return next(error);
    }
  }

  public async getById(req: Request, res: Response, next: NextFunction) {
    try {
      const taskRequestResult = await this.taskRequestService.getById(req.params.id);

      if (taskRequestResult.isFailure) {
        return res.status(400).json({ message: taskRequestResult.error.toString() });
      }

      const taskRequestDTO: ITaskRequestDTO = taskRequestResult.getValue();

      return res.status(200).json(taskRequestDTO);
    } catch (error) {
      return next(error);
    }
  }
}
