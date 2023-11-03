import { Container } from 'typedi';
import { UniqueEntityID } from '../../src/core/domain/UniqueEntityID';
import { Result } from '../../src/core/logic/Result';
import { Floor } from '../../src/domain/Floor/floor';
import IFloorDTO from '../../src/dto/IFloorDTO';
import { FloorMap } from '../../src/mappers/FloorMap';
import IFloorRepo from '../../src/services/IRepos/IFloorRepo';
import IBuildingService from '../../src/services/IServices/IBuildingService';
import FloorService from '../../src/services/floorService';

describe('FloorService', () => {
  let floorService: FloorService;
  let floorRepoMock: jest.Mocked<IFloorRepo>;
  let buildingServiceMock: jest.Mocked<IBuildingService>;
  let floorStub: Floor;

  beforeEach(() => {
    floorRepoMock = {
      save: jest.fn(),
      exists: jest.fn(),
      findByCode: jest.fn(),
      findById: jest.fn(),
      findByBuildingCode: jest.fn(),
      findByCodes: jest.fn(),
      findAllFloors: jest.fn(),
    };

    buildingServiceMock = {
      createBuilding: jest.fn(),
      updateBuilding: jest.fn(),
      getBuildingByCode: jest.fn(),
      getAllBuildings: jest.fn(),
      getBuildingsByFloorRange: jest.fn(),
    };

    floorStub = {
      id: new UniqueEntityID(),
      floorNumber: 1,
      description: { value: 'Test floor' },
      servedByElevator: true,
      buildingCode: { value: 'B1' },
    } as Floor;

    Container.set('floorRepo', floorRepoMock);
    Container.set('buildingService', buildingServiceMock);
    floorService = new FloorService(floorRepoMock, buildingServiceMock);
  });

  describe('createFloor', () => {
    it('should successfully create a floor', async () => {
      const floorDTO: IFloorDTO = {
        id: '00000000-0000-0000-0000-000000000000',
        floorNumber: 1,
        description: 'Test floor',
        servedByElevator: true,
        buildingCode: 'B1',
      };

      floorRepoMock.save.mockResolvedValue(floorStub as any);
      buildingServiceMock.getBuildingByCode.mockResolvedValue(Result.ok(true) as any);

      const result = await floorService.createFloor(floorDTO);

      expect(result.isSuccess).toBe(true);
      expect(floorRepoMock.save).toBeCalled();
    });

    it('should fail to create a floor when input is invalid', async () => {
      const floorDTO: IFloorDTO = {
        id: '00000000-0000-0000-0000-000000000000',
        floorNumber: 1,
        description: 'Test floor',
        servedByElevator: true,
        buildingCode: '--------------------------',
      };

      const result = await floorService.createFloor(floorDTO);

      expect(result.isFailure).toBe(true);
      expect(floorRepoMock.save).not.toBeCalled();
    });
  });
  describe('updateFloor', () => {
    it('should successfully update a floor', async () => {
      const floorId = floorStub.id.toString();
      const updatedFloorDTO: IFloorDTO = {
        id: floorId,
        floorNumber: floorStub.floorNumber,
        description: floorStub.description.value,
        servedByElevator: floorStub.servedByElevator,
        buildingCode: floorStub.buildingCode.value,
      };

      floorRepoMock.findById.mockResolvedValue(floorStub as any);
      floorRepoMock.save.mockResolvedValue(floorStub as any);
      floorRepoMock.exists.mockResolvedValue(true as any);
      buildingServiceMock.getBuildingByCode.mockResolvedValue(Result.ok(true) as any);

      const result = await floorService.updateFloor(updatedFloorDTO);

      expect(result.isSuccess).toBe(true);
      expect(buildingServiceMock.getBuildingByCode).toBeCalledWith(updatedFloorDTO.buildingCode);
      expect(floorRepoMock.save).toHaveBeenCalled();
    });

    it('should fail to update a floor when floor is not found', async () => {
      const floorId = '00000000-0000-0000-0000-000000000000';
      const updatedFloorDTO: IFloorDTO = {
        id: floorId,
        floorNumber: 2,
        description: 'Updated floor',
        servedByElevator: true,
        buildingCode: 'B2',
      };

      floorRepoMock.exists.mockResolvedValue(false as any);
      buildingServiceMock.getBuildingByCode.mockResolvedValue(Result.ok(true) as any);

      const result = await floorService.updateFloor(updatedFloorDTO);

      expect(result.isFailure).toBe(true);
      expect(floorRepoMock.exists).toBeCalledWith(FloorMap.toDomain(updatedFloorDTO).getValue());
      expect(floorRepoMock.save).not.toBeCalled();
    });
  });

  describe('partialUpdateFloor', () => {
    it('should successfully partially update a floor', async () => {
      const floorId = '00000000-0000-0000-0000-000000000000';
      const updatedFloorDTO: Partial<IFloorDTO> = {
        id: floorId,
        description: 'Updated floor',
      };

      floorRepoMock.findById.mockResolvedValue(floorStub as any);
      floorRepoMock.save.mockResolvedValue(floorStub as any);

      const result = await floorService.partialUpdateFloor(updatedFloorDTO);

      expect(result.isSuccess).toBe(true);
      expect(floorRepoMock.findById).toBeCalledWith(floorId);
      expect(floorRepoMock.save).toBeCalledWith(expect.anything());
    });
  });
});
