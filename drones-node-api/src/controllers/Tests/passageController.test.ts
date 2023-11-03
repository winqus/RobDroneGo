import { NextFunction, Request, Response } from 'express';
import { MockProxy, mock } from 'jest-mock-extended';
import { Container } from 'typedi';
import { Result } from '../../core/logic/Result';
import IPassageDTO from '../../dto/IPassageDTO';
import IPassageService from '../../services/IServices/IPassageService';
import PassageController from '../passageController';

describe('PassageController', () => {
  let passageController: PassageController;
  let passageServiceMock: MockProxy<IPassageService>;
  let reqMock: MockProxy<Request>;
  let resMock: MockProxy<Response>;
  let nextMock: MockProxy<NextFunction>;

  beforeEach(() => {
    passageServiceMock = mock<IPassageService>();

    reqMock = mock<Request>({ body: {} });
    resMock = mock<Response>({
      status: jest.fn().mockReturnThis(),
      json: jest.fn().mockReturnThis(),
    });
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

  describe('getPassages', () => {
    let passageDTOstub: IPassageDTO;
    let passagesStub: IPassageDTO[];

    beforeEach(() => {
      passageDTOstub = {
        id: '000',
        buildingCode1: 'Aaa',
        buildingCode2: 'Baa',
        floorNumber1: 3,
        floorNumber2: 3,
      };

      passagesStub = [
        passageDTOstub,
        {
          ...passageDTOstub,
          id: '111',
          floorNumber1: 2,
          floorNumber2: 2,
        },
      ];
    });

    it('should return 200 and a list of passages between two buildings if building codes are provided', async () => {
      const passages: IPassageDTO[] = passagesStub;
      reqMock.query = {
        buildingCode1: 'Aaa',
        buildingCode2: 'Bbb',
      };
      passageServiceMock.getPassagesBetweenBuildings.mockResolvedValue(Result.ok<IPassageDTO[]>(passages) as any);

      await passageController.getPassages(reqMock as Request, resMock as Response, nextMock);

      expect(passageServiceMock.getPassagesBetweenBuildings).toHaveBeenCalledWith('Aaa', 'Bbb');
      expect(resMock.status).toHaveBeenCalledWith(200);
      expect(resMock.json).toHaveBeenCalledWith(passages);
    });

    it('should return 200 and a list of all passages if no building codes are provided', async () => {
      const passages: IPassageDTO[] = passagesStub;
      reqMock.query = {};
      passageServiceMock.getAllPassages.mockResolvedValue(Result.ok<IPassageDTO[]>(passages) as any);

      await passageController.getPassages(reqMock as Request, resMock as Response, nextMock);

      expect(passageServiceMock.getAllPassages).toHaveBeenCalled();
      expect(resMock.status).toHaveBeenCalledWith(200);
      expect(resMock.json).toHaveBeenCalledWith(passages);
    });

    it('should return 400 if there is a failure in getting passages', async () => {
      reqMock.query = {
        buildingCode1: 'A',
        buildingCode2: 'B',
      };
      passageServiceMock.getPassagesBetweenBuildings.mockResolvedValue(
        Result.fail<IPassageDTO[]>('Some error occurred') as any,
      );

      await passageController.getPassages(reqMock as Request, resMock as Response, nextMock);

      expect(resMock.status).toHaveBeenCalledWith(400);
      expect(resMock.json).toHaveBeenCalledWith({ message: 'Some error occurred' });
    });

    it('should pass the error to next if an exception occurs', async () => {
      const error = new Error('Unexpected error');
      reqMock.query = {};
      passageServiceMock.getAllPassages.mockRejectedValue(error);

      await passageController.getPassages(reqMock as Request, resMock as Response, nextMock);

      expect(nextMock).toHaveBeenCalledWith(error);
    });
  });
});
