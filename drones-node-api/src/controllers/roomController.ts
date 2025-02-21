import { NextFunction, Request, Response } from 'express';
import { Inject, Service } from 'typedi';
import config from '../../config';
import { Result } from '../core/logic/Result';
import IRoomDTO from '../dto/IRoomDTO';
import IRoomService from '../services/IServices/IRoomService';
import IRoomController from './IControllers/IRoomController';

@Service()
export default class RoomController implements IRoomController {
  constructor(@Inject(config.services.room.name) private roomService: IRoomService) {}

  public async createRoom(req: Request, res: Response, next: NextFunction) {
    try {
      const roomOrError = (await this.roomService.createRoom(req.body as IRoomDTO)) as Result<IRoomDTO>;

      if (roomOrError.isFailure) {
        return res.status(400).json({ message: roomOrError.error.toString() });
      }

      const roomDTO = roomOrError.getValue();

      return res.status(201).json(roomDTO);
    } catch (error) {
      return next(error);
    }
  }

  public async getAllRooms(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const result = await this.roomService.getAllRooms();

      if (result.isSuccess) {
        res.status(200).json(result.getValue());
      } else {
        res.status(400).json({ error: result.error });
      }
    } catch (error) {
      next(error);
    }
  }
}
