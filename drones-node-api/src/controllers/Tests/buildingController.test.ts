import { NextFunction, Request, Response } from 'express';
import { MockProxy, mock } from 'jest-mock-extended';
import { Container } from 'typedi';
import { Result } from '../../core/logic/Result';
import { Building } from '../../domain/Building/building';
import IBuildingDTO from '../../dto/IBuildingDTO';
import IElevatorDTO from '../../dto/IElevatorDTO';
import IBuildingService from '../../services/IServices/IBuildingService';
import IElevatorService from '../../services/IServices/IElevadorService';
import BuildingController from '../buildingController';

describe('BuildingController', () => {
  let buildingController: BuildingController;
  let buildingServiceMock: MockProxy<IBuildingService>;
  let elevatorServiceMock: MockProxy<IElevatorService>;
  let reqMock: MockProxy<Request>;
  let resMock: MockProxy<Response>;
  let nextMock: MockProxy<NextFunction>;

  beforeEach(() => {
    buildingServiceMock = mock<IBuildingService>();
    elevatorServiceMock = mock<IElevatorService>();

    reqMock = mock<Request>({ body: {} });
    resMock = mock<Response>({
      status: jest.fn().mockReturnThis(),
      json: jest.fn().mockReturnThis(),
    });
    nextMock = jest.fn();

    Container.set('buildingService', buildingServiceMock);
    buildingController = new BuildingController(
      buildingServiceMock as IBuildingService,
      elevatorServiceMock as IElevatorService,
    );
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

  describe('updateBuilding', () => {
    it('should successfully update a building and return 200 status', async () => {
      const buildingDTO: IBuildingDTO = {
        id: '00000000-0000-0000-0000-000000000000',
        name: 'Building1',
        code: 'A1',
        description: 'Test building',
        floorSizeLength: 100,
        floorSizeWidth: 200,
      };

      const updatedBuildingDTO: IBuildingDTO = {
        ...buildingDTO,
        name: 'Building2',
      };

      reqMock.params = {
        id: '00000000-0000-0000-0000-000000000000',
      };

      buildingServiceMock.updateBuilding.mockResolvedValue(Result.ok<IBuildingDTO>(updatedBuildingDTO) as any);

      await buildingController.updateBuilding(reqMock as Request, resMock as Response, nextMock);

      expect(resMock.status).toHaveBeenCalledWith(200);
      expect(resMock.json).toHaveBeenCalledWith(updatedBuildingDTO);
    });

    it('should return 400 status if building update fails', async () => {
      buildingServiceMock.updateBuilding.mockResolvedValue(Result.fail<IBuildingDTO>('An error occurred') as any);

      reqMock.params = {
        id: '00000000-0000-0000-0000-000000000000',
      };

      await buildingController.updateBuilding(reqMock as Request, resMock as Response, nextMock);

      expect(resMock.status).toHaveBeenCalledWith(400);
      expect(resMock.json).toHaveBeenCalledWith({ message: 'An error occurred' });
    });
  });

  describe('ElevatorController', () => {
    it('should create an elevator and return 201 status', async () => {
      reqMock.body = {
        number: 1,
        make: 'Make',
        model: 'Model',
        serialNumber: 'SN123',
        description: 'Elevator Description',
        floors: [1, 2, 3],
      };
      reqMock.params = {
        code: 'A',
      };

      const elevatorDTO: IElevatorDTO = {
        id: '00000000-0000-0000-0000-000000000000',
        number: 1,
        make: 'Make',
        model: 'Model',
        serialNumber: 'SN123',
        description: 'Elevator Description',
      };
      const elevatorResult = Result.ok(elevatorDTO);

      const buildingDTO = {
        id: '00000000-0000-0000-0000-000000000000',
        name: 'Building1',
        code: 'A',
        description: 'Test building',
        floorSizeLength: 100,
        floorSizeWidth: 200,
      } as IBuildingDTO;

      elevatorServiceMock.createElevator.mockReturnValue(Promise.resolve(Result.ok(buildingDTO)) as any);

      await buildingController.createElevator(reqMock as any, resMock as any, nextMock);

      expect(resMock.status).toHaveBeenCalledWith(201);
      expect(resMock.json).toHaveBeenCalledWith(buildingDTO);
    });

    it('should handle a failure and return 400 status with an error message', async () => {
      reqMock.body = {
        number: 1,
        make: 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
        model: 'Model',
        serialNumber: 'SN123',
        description: 'Elevator Description',
        floors: [1, 2, 3],
      };
      reqMock.params = {
        code: 'BuildingCode',
      };

      const errorMessage = 'Invalid elevator data';
      const elevatorResult = Result.fail(errorMessage);

      elevatorServiceMock.createElevator.mockResolvedValue(elevatorResult as any);

      await buildingController.createElevator(reqMock as any, resMock as any, nextMock);

      expect(resMock.status).toHaveBeenCalledWith(400);
      expect(resMock.json).toHaveBeenCalledWith({ message: errorMessage });
    });

    it('should update an elevator and return 200 status', async () => {
      reqMock.body = {
        number: 1,
        make: 'Make',
        model: 'Model',
        serialNumber: 'SN123',
        description: 'Elevator Description',
      };
      reqMock.params = {
        code: 'BuildingCode',
      };

      const elevatorDTO: IElevatorDTO = {
        id: '00000000-0000-0000-0000-000000000000',
        number: 1,
        make: 'Make',
        model: 'Model',
        serialNumber: 'SN123',
        description: 'Elevator Description',
      };

      elevatorServiceMock.updateElevator.mockResolvedValue(Result.ok(elevatorDTO) as any);

      await buildingController.updateElevator(reqMock as any, resMock as any, nextMock);

      expect(resMock.status).toHaveBeenCalledWith(200);
      expect(resMock.json).toHaveBeenCalledWith(elevatorDTO);
    });
  });

  describe('listAllBuildings', () => {
    it('should successfully get all buildings and return 200 status', async () => {
      const buildingDTO: IBuildingDTO = {
        id: '00000000-0000-0000-0000-000000000000',
        name: 'Building1',
        code: 'A1',
        description: 'Test building',
        floorSizeLength: 100,
        floorSizeWidth: 200,
      };

      buildingServiceMock.getAllBuildings.mockResolvedValue(
        Result.ok<IBuildingDTO[]>([buildingDTO]) as any,
      );

      await buildingController.listAllBuildings(reqMock as Request, resMock as Response, nextMock);

      expect(resMock.status).toHaveBeenCalledWith(200);
      expect(resMock.json).toHaveBeenCalledWith([buildingDTO]);
    });

    it('should return 400 status if there is a failure in getting buildings', async () => {
      buildingServiceMock.getAllBuildings.mockResolvedValue(Result.fail<IBuildingDTO[]>('An error occurred') as any);

      await buildingController.listAllBuildings(reqMock as Request, resMock as Response, nextMock);

      expect(resMock.status).toHaveBeenCalledWith(404);
      expect(resMock.json).toHaveBeenCalledWith({ message: 'An error occurred' });
    });
  });
});
