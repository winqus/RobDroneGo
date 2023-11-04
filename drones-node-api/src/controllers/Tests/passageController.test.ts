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

  describe('PassageController - listFloorsWithPassagesToDifferentBuilding', () => {
    let passageController: PassageController;
    let passageServiceMock: MockProxy<IPassageService>;
    let reqMock: MockProxy<Request>;
    let resMock: MockProxy<Response>;
    let nextMock: MockProxy<NextFunction>;

    beforeEach(() => {
      passageServiceMock = mock<IPassageService>();
      reqMock = mock<Request>({ query: {} });
      resMock = mock<Response>({
        status: jest.fn().mockReturnThis(),
        json: jest.fn().mockReturnThis(),
      });
      nextMock = jest.fn();

      Container.set('passageService', passageServiceMock);
      passageController = new PassageController(passageServiceMock as IPassageService);
    });

    it('should successfully list floors with passages to different buildings', async () => {
      const buildingCode = 'BA';
      const listFloorsDTO = [{ floorName: 'Floor A' }, { floorName: 'Floor B' }];

      passageServiceMock.listFloorsWithPassagesToDifferentBuilding.mockResolvedValue(Result.ok(listFloorsDTO) as any);

      await passageController.listFloorsWithPassagesToDifferentBuilding(reqMock, resMock, nextMock);

      expect(resMock.status).toHaveBeenCalledWith(200);
      expect(resMock.json).toHaveBeenCalledWith(listFloorsDTO);
    });

    it('should return 400 status if listing fails', async () => {
      const buildingCode = 'BA';

      passageServiceMock.listFloorsWithPassagesToDifferentBuilding.mockResolvedValue(
        Result.fail('An error occurred') as any,
      );

      await passageController.listFloorsWithPassagesToDifferentBuilding(reqMock, resMock, nextMock);

      expect(resMock.status).toHaveBeenCalledWith(400);
      expect(resMock.json).toHaveBeenCalledWith({ message: 'An error occurred' });
    });

    it('should pass the error to next if an exception occurs', async () => {
      const error = new Error('Unexpected error');
      passageServiceMock.listFloorsWithPassagesToDifferentBuilding.mockRejectedValue(error);

      await passageController.listFloorsWithPassagesToDifferentBuilding(reqMock, resMock, nextMock);

      expect(nextMock).toHaveBeenCalledWith(error);
    });
  });

  describe('PassageController - updatePassage', () => {
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

      passageController = new PassageController(passageServiceMock);
    });

    it('should successfully update a passage and return 200 status', async () => {
      const oldPassageDTO: IPassageDTO = {
        id: '00000000-0000-0000-0000-000000000000',
        buildingCode1: 'A',
        buildingCode2: 'B',
        floorNumber1: 2,
        floorNumber2: 2,
      };

      const newPassageDTO: IPassageDTO = {
        id: '00000000-0000-0000-0000-000000000001',
        buildingCode1: 'C',
        buildingCode2: 'D',
        floorNumber1: 3,
        floorNumber2: 3,
      };

      const updatedPassageDTO = {
        id: newPassageDTO.id,
        buildingCode1: newPassageDTO.buildingCode1,
        buildingCode2: newPassageDTO.buildingCode2,
        floorNumber1: newPassageDTO.floorNumber1,
        floorNumber2: newPassageDTO.floorNumber2,
      };

      passageServiceMock.updatePassage.mockResolvedValue(Result.ok<IPassageDTO>(updatedPassageDTO) as any);

      reqMock.body = { oldPassage: oldPassageDTO, newPassage: newPassageDTO };

      await passageController.updatePassage(reqMock, resMock, nextMock);

      expect(resMock.status).toHaveBeenCalledWith(200);
      expect(resMock.json).toHaveBeenCalledWith(updatedPassageDTO);
    });

    it('should return 400 status if passage update fails', async () => {
      const oldPassageDTO: IPassageDTO = {
        id: '00000000-0000-0000-0000-000000000000',
        buildingCode1: 'A',
        buildingCode2: 'B',
        floorNumber1: 2,
        floorNumber2: 2,
      };

      const newPassageDTO: IPassageDTO = {
        id: '00000000-0000-0000-0000-000000000001',
        buildingCode1: 'C',
        buildingCode2: 'D',
        floorNumber1: 3,
        floorNumber2: 3,
      };

      passageServiceMock.updatePassage.mockResolvedValue(Result.fail<IPassageDTO>('An error occurred') as any);

      reqMock.body = { oldPassage: oldPassageDTO, newPassage: newPassageDTO };

      await passageController.updatePassage(reqMock, resMock, nextMock);

      expect(resMock.status).toHaveBeenCalledWith(400);
      expect(resMock.json).toHaveBeenCalledWith({ message: 'An error occurred' });
    });
  });
});
