import { NextFunction, Request, Response } from 'express';
import { MockProxy, mock } from 'jest-mock-extended';
import { UniqueEntityID } from '../../core/domain/UniqueEntityID';
import { Result } from '../../core/logic/Result';
import { RoomCategory } from '../../domain/Room/ValueObjects/category';
import IRoomDTO from '../../dto/IRoomDTO';
import IRoomService from '../../services/IServices/IRoomService';
import RoomController from '../roomController';

const roomDTOstub = {
  name: 'Room 1',
  description: 'Room 1 description',
  category: RoomCategory.Amphitheater.valueOf(),
  size: {
    width: 10,
    length: 10,
  },
  position: {
    x: 0,
    y: 0,
  },
  floorId: new UniqueEntityID().toString(),
};

describe('RoomController', () => {
  let roomController: RoomController;

  let roomServiceMock: MockProxy<IRoomService>;
  let reqMock: MockProxy<Request>;
  let resMock: MockProxy<Response>;
  let nextMock: MockProxy<NextFunction>;

  beforeEach(() => {
    reqMock = mock<Request>({ body: {} });
    resMock = mock<Response>({
      status: jest.fn().mockReturnThis(),
      json: jest.fn().mockReturnThis(),
    });
    nextMock = jest.fn();

    roomServiceMock = mock<IRoomService>();
    roomController = new RoomController(roomServiceMock as IRoomService);
  });

  describe('createRoom', () => {
    it('should successfully create a room and return 201 status', async () => {
      roomServiceMock.createRoom.mockResolvedValue(Result.ok<IRoomDTO>(roomDTOstub as any) as any);

      await roomController.createRoom(reqMock as Request, resMock as Response, nextMock);

      expect(resMock.status).toHaveBeenCalledWith(201);
      expect(resMock.json).toHaveBeenCalledWith(roomDTOstub);
    });

    it('should return 400 status if room creation fails', async () => {
      roomServiceMock.createRoom.mockResolvedValue(Result.fail<IRoomDTO>('An error occurred') as any);

      await roomController.createRoom(reqMock as Request, resMock as Response, nextMock);

      expect(resMock.status).toHaveBeenCalledWith(400);
      expect(resMock.json).toHaveBeenCalledWith({ message: 'An error occurred' });
    });
  });
});
