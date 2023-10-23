import { NextFunction, Request, Response } from 'express';
import { Container } from 'typedi';
import { Result } from '../../core/logic/Result';
import IBuildingDTO from '../../dto/IBuildingDTO';
import IBuildingService from '../../services/IServices/IBuildingService';
import BuildingController from '../buildingController';

describe('BuildingController', () => {
  let buildingController: BuildingController;
  let buildingServiceMock: jest.Mocked<IBuildingService>;
  let reqMock: Partial<Request>;
  let resMock: Partial<Response>;
  let nextMock: NextFunction;

  beforeEach(() => {
    buildingServiceMock = {
      createBuilding: jest.fn(),
    };

    reqMock = {
      body: {},
    };

    resMock = {
      status: jest.fn().mockReturnThis(),
      json: jest.fn().mockReturnThis(),
    };

    nextMock = jest.fn();

    Container.set('buildingService', buildingServiceMock);
    buildingController = new BuildingController(buildingServiceMock as IBuildingService);
  });

  describe('createBuilding', () => {
    it('should successfully create a building and return 201 status', async () => {
      const buildingDTO: IBuildingDTO = {
        id: '00000000-0000-0000-0000-000000000000',
        name: 'Building1',
        code: 'A1',
        description: 'Test building',
        floorSizeLength: 100,
        floorSizeWidth: 200,
      };

      buildingServiceMock.createBuilding.mockResolvedValue(Result.ok<IBuildingDTO>(buildingDTO) as any);

      await buildingController.createBuilding(reqMock as Request, resMock as Response, nextMock);

      expect(resMock.status).toHaveBeenCalledWith(201);
      expect(resMock.json).toHaveBeenCalledWith(buildingDTO);
    });

    it('should return 400 status if building creation fails', async () => {
      buildingServiceMock.createBuilding.mockResolvedValue(Result.fail<IBuildingDTO>('An error occurred') as any);

      await buildingController.createBuilding(reqMock as Request, resMock as Response, nextMock);

      expect(resMock.status).toHaveBeenCalledWith(400);
      expect(resMock.json).toHaveBeenCalledWith({ message: 'An error occurred' });
    });
  });
});
