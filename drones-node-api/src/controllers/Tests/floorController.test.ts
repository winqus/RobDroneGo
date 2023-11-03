import { NextFunction, Request, Response } from 'express';
import { Container } from 'typedi';
import { Result } from '../../core/logic/Result';
import IFloorDTO from '../../dto/IFloorDTO';
import IBuildingService from '../../services/IServices/IBuildingService';
import IFloorService from '../../services/IServices/IFloorService';
import FloorController from '../floorController';

describe('FloorController', () => {
  let floorController: FloorController;
  let floorServiceMock: jest.Mocked<IFloorService>;
  let buildingServiceMock: jest.Mocked<IBuildingService>;
  let reqMock: Partial<Request>;
  let resMock: Partial<Response>;
  let nextMock: NextFunction;

  beforeEach(() => {
    floorServiceMock = {
      createFloor: jest.fn(),
      updateFloor: jest.fn(),
      partialUpdateFloor: jest.fn(),
      getAllFloors: jest.fn(),
      getFloorsByBuildingCode: jest.fn(),
    } as jest.Mocked<IFloorService>;

    buildingServiceMock = {
      createBuilding: jest.fn(),
      updateBuilding: jest.fn(),
      getBuildingByCode: jest.fn(),
      getAllBuildings: jest.fn(),
      getBuildingsByFloorRange: jest.fn(),
    };

    reqMock = {
      body: {},
    };

    resMock = {
      status: jest.fn().mockReturnThis(),
      json: jest.fn().mockReturnThis(),
    };

    nextMock = jest.fn();

    Container.set('floorService', floorServiceMock);
    Container.set('buildingService', buildingServiceMock);
    floorController = new FloorController(floorServiceMock);
  });

  describe('createFloor', () => {
    it('should successfully create a floor and return 201 status', async () => {
      const floorDTO: IFloorDTO = {
        id: '00000000-0000-0000-0000-000000000000',
        floorNumber: 12,
        description: 'Test floor',
        servedByElevator: true,
        buildingCode: 'B1',
      };

      buildingServiceMock.getBuildingByCode.mockResolvedValue(Result.ok('Building exists') as any);
      floorServiceMock.createFloor.mockResolvedValue(Result.ok<IFloorDTO>(floorDTO) as any);

      await floorController.createFloor(reqMock as Request, resMock as Response, nextMock);

      expect(resMock.status).toHaveBeenCalledWith(201);
      expect(resMock.json).toHaveBeenCalledWith(floorDTO);
    });

    it('should return 400 status if floor creation fails', async () => {
      buildingServiceMock.getBuildingByCode.mockResolvedValue(Result.ok('Building exists') as any);
      floorServiceMock.createFloor.mockResolvedValue(Result.fail<IFloorDTO>('Error creating floor') as any);

      await floorController.createFloor(reqMock as Request, resMock as Response, nextMock);

      expect(resMock.status).toHaveBeenCalledWith(400);
      expect(resMock.json).toHaveBeenCalledWith({ message: 'Error creating floor' });
    });
  });

  describe('updateFloor', () => {
    it('should successfully update a floor and return 200 status', async () => {
      const floorDTO: IFloorDTO = {
        id: '00000000-0000-0000-0000-000000000000',
        floorNumber: 12,
        description: 'Updated floor',
        servedByElevator: true,
        buildingCode: 'B1',
      };

      reqMock.params = { floorId: floorDTO.id };
      reqMock.body = floorDTO;

      floorServiceMock.updateFloor.mockResolvedValue(Result.ok<IFloorDTO>(floorDTO) as any);

      await floorController.updateFloor(reqMock as Request, resMock as Response, nextMock);

      expect(resMock.status).toHaveBeenCalledWith(200);
      expect(resMock.json).toHaveBeenCalledWith(floorDTO);
    });

    it('should return 400 status if floor update fails', async () => {
      const floorDTO: IFloorDTO = {
        id: '00000000-0000-0000-0000-000000000000',
        floorNumber: 12,
        description: 'Updated floor',
        servedByElevator: true,
        buildingCode: 'B1',
      };

      reqMock.params = { floorId: floorDTO.id };
      reqMock.body = floorDTO;

      floorServiceMock.updateFloor.mockResolvedValue(Result.fail<IFloorDTO>('Error updating floor') as any);

      await floorController.updateFloor(reqMock as Request, resMock as Response, nextMock);

      expect(resMock.status).toHaveBeenCalledWith(400);
      expect(resMock.json).toHaveBeenCalledWith({ message: 'Error updating floor' });
    });
  });

  describe('partialUpdateFloor', () => {
    it('should successfully partially update a floor and return 200 status', async () => {
      const floorDTO: IFloorDTO = {
        id: '00000000-0000-0000-0000-000000000000',
        floorNumber: 12,
        description: 'Partially Updated floor',
        servedByElevator: true,
        buildingCode: 'B1',
      };

      reqMock.params = { floorId: floorDTO.id };
      reqMock.body = floorDTO;

      floorServiceMock.partialUpdateFloor.mockResolvedValue(Result.ok<IFloorDTO>(floorDTO) as any);

      await floorController.partialUpdateFloor(reqMock as Request, resMock as Response, nextMock);

      expect(resMock.status).toHaveBeenCalledWith(200);
      expect(resMock.json).toHaveBeenCalledWith(floorDTO);
    });

    it('should return 400 status if floor partial update fails', async () => {
      const floorDTO: IFloorDTO = {
        id: '00000000-0000-0000-0000-000000000000',
        floorNumber: 12,
        description: 'Partially Updated floor',
        servedByElevator: true,
        buildingCode: 'B1',
      };

      reqMock.params = { floorId: floorDTO.id };
      reqMock.body = floorDTO;

      floorServiceMock.partialUpdateFloor.mockResolvedValue(
        Result.fail<IFloorDTO>('Error partially updating floor') as any,
      );

      await floorController.partialUpdateFloor(reqMock as Request, resMock as Response, nextMock);

      expect(resMock.status).toHaveBeenCalledWith(400);
      expect(resMock.json).toHaveBeenCalledWith({ message: 'Error partially updating floor' });
    });
  });

  describe('getFloorsByBuildingCode', () => {
    it('should successfully get floors by building code and return 200 status', async () => {
      const floorDTO: IFloorDTO = {
        id: '00000000-0000-0000-0000-000000000000',
        floorNumber: 12,
        description: 'Partially Updated floor',
        servedByElevator: true,
        buildingCode: 'B1',
      };

      reqMock.params = { buildingCode: floorDTO.buildingCode };

      floorServiceMock.getFloorsByBuildingCode.mockResolvedValue(
        Result.ok<IFloorDTO[]>([floorDTO]) as any,
      );

      await floorController.getFloorsByBuildingCode(reqMock as Request, resMock as Response, nextMock);

      expect(resMock.status).toHaveBeenCalledWith(200);
      expect(resMock.json).toHaveBeenCalledWith([floorDTO]);
    });

    it('should return 400 status if there is a failure in getting floors', async () => {
      reqMock.params = { buildingCode: 'B1' };

      floorServiceMock.getFloorsByBuildingCode.mockResolvedValue(
        Result.fail<IFloorDTO[]>('Error getting floors by building code') as any,
      );

      await floorController.getFloorsByBuildingCode(reqMock as Request, resMock as Response, nextMock);

      expect(resMock.status).toHaveBeenCalledWith(404);
      expect(resMock.json).toHaveBeenCalledWith({ message: 'Error getting floors by building code' });
    });
  });
});
