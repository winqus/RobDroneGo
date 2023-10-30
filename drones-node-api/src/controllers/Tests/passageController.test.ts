import { NextFunction, Request, Response } from 'express';
import { Container } from 'typedi';
import { Result } from '../../core/logic/Result';
import IPassageDTO from '../../dto/IPassageDTO';
import IPassageService from '../../services/IServices/IPassageService';
import PassageController from '../passageController';

describe('PassageController', () => {
  let passageController: PassageController;
  let passageServiceMock: jest.Mocked<IPassageService>;
  let reqMock: Partial<Request>;
  let resMock: Partial<Response>;
  let nextMock: NextFunction;

  beforeEach(() => {
    passageServiceMock = {
      createPassage: jest.fn(),
    };

    reqMock = {
      body: {},
    };

    resMock = {
      status: jest.fn().mockReturnThis(),
      json: jest.fn().mockReturnThis(),
    };

    nextMock = jest.fn();

    Container.set('passageService', passageServiceMock);
    passageController = new PassageController(passageServiceMock as IPassageService);
  });

  describe('createPassage', () => {
    it('should successfully create a passage and return 201 status', async () => {
      const passageDTO: IPassageDTO = {
        id: '00000000-0000-0000-0000-000000000000',
        buildingCode1: 'A',
        buildingCode2: 'B',
        floorNumber1: 2,
        floorNumber2: 2,
      };

      passageServiceMock.createPassage.mockResolvedValue(Result.ok<IPassageDTO>(passageDTO) as any);

      await passageController.createPassage(reqMock as Request, resMock as Response, nextMock);

      expect(resMock.status).toHaveBeenCalledWith(201);
      expect(resMock.json).toHaveBeenCalledWith(passageDTO);
    });

    it('should return 400 status if passage creation fails', async () => {
      passageServiceMock.createPassage.mockResolvedValue(Result.fail<IPassageDTO>('An error occurred') as any);

      await passageController.createPassage(reqMock as Request, resMock as Response, nextMock);

      expect(resMock.status).toHaveBeenCalledWith(400);
      expect(resMock.json).toHaveBeenCalledWith({ message: 'An error occurred' });
    });
  });
});
