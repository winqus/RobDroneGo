import { MockProxy, mock } from 'jest-mock-extended';
import { UniqueEntityID } from '../../src/core/domain/UniqueEntityID';
import { Result } from '../../src/core/logic/Result';
import { Floor } from '../../src/domain/Floor/floor';
import IFloorDTO from '../../src/dto/IFloorDTO';
import { FloorMap } from '../../src/mappers/FloorMap';
import IBuildingRepo from '../../src/services/IRepos/IBuildingRepo';
import IFloorRepo from '../../src/services/IRepos/IFloorRepo';
import IBuildingService from '../../src/services/IServices/IBuildingService';
import FloorService from '../../src/services/floorService';

describe('FloorService', () => {
  let floorService: FloorService;
  let floorRepoMock: MockProxy<IFloorRepo>;
  let buildingRepoMock: MockProxy<IBuildingRepo>;
  let buildingServiceMock: MockProxy<IBuildingService>;
  let floorStub: Floor;

  beforeEach(() => {
    floorRepoMock = mock<IFloorRepo>();
    buildingRepoMock = mock<IBuildingRepo>();
    buildingServiceMock = mock<IBuildingService>();

    floorStub = {
      id: new UniqueEntityID(),
      floorNumber: 1,
      description: { value: 'Test floor' },
      servedByElevator: true,
      buildingCode: { value: 'B1' },
    } as Floor;

    floorService = new FloorService(floorRepoMock, buildingServiceMock, buildingRepoMock);
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

  describe('getAllFloors', () => {
    it('should successfully get all floors', async () => {
      floorRepoMock.findAllFloors.mockResolvedValue([floorStub] as any);

      const result = await floorService.getAllFloors();

      expect(result.isSuccess).toBe(true);
      expect(floorRepoMock.findAllFloors).toBeCalled();
    });
  });

  describe('getFloorsByBuildingCode', () => {
    it('should successfully get floors by building code', async () => {
      const buildingCode = 'B1';

      floorRepoMock.findByBuildingCode.mockResolvedValue([floorStub] as any);

      const result = await floorService.getFloorsByBuildingCode(buildingCode);

      expect(result.isSuccess).toBe(true);
      expect(floorRepoMock.findByBuildingCode).toBeCalledWith(buildingCode);
    });
  });

  describe('getFloorsServedByElevator', () => {
    it('should successfully get floors served by elevator', async () => {
      const buildingCode = 'B1';

      floorRepoMock.findByBuildingCode.mockResolvedValue([floorStub] as any);

      const result = await floorService.getFloorsServedByElevator(buildingCode);

      expect(result.isSuccess).toBe(true);
      expect(floorRepoMock.findByBuildingCode).toBeCalledWith(buildingCode);
    });

    it('should get an empty array when no floors are served by elevator', async () => {
      const buildingCode = 'B1';

      floorRepoMock.findByBuildingCode.mockResolvedValue([] as any);

      const result = await floorService.getFloorsServedByElevator(buildingCode);

      expect(result.isSuccess).toBe(true);
      expect(floorRepoMock.findByBuildingCode).toBeCalledWith(buildingCode);
    });

    it('should fail to get floors served by elevator when building code is invalid', async () => {
      const buildingCode = null;

      const result = await floorService.getFloorsServedByElevator(buildingCode as any);

      expect(result.isFailure).toBe(true);
    });
  });

  describe('loadMap', () => {
    it('should successfully load a map for a floor', async () => {
      const buildingCode = 'B1';
      const floorNumber = 1;
      const mapData = {
        size: { width: 2, height: 2 },
        map: [
          [1, 2],
          [3, 4],
        ],
      };

      const buildingMock = {
        code: { value: buildingCode },
        floorSize: { value: { width: 2, height: 2 } },
      };
      const floorMock = {
        id: new UniqueEntityID(),
        buildingCode: { value: buildingCode },
        floorNumber: 1,
        description: { value: 'Test floor' },
        servedByElevator: true,
        map: {
          size: { width: 2, height: 2 },
          map: [
            [0, 0],
            [0, 0],
          ],
        },
      };

      buildingRepoMock.findByCode.mockResolvedValue(buildingMock as any);
      floorRepoMock.findByCode.mockResolvedValue(floorMock as any);
      floorRepoMock.save.mockResolvedValue(floorMock as any);

      const result = await floorService.loadMap(buildingCode, floorNumber, mapData);

      expect(result.isSuccess).toBe(true);
      expect(buildingRepoMock.findByCode).toBeCalledWith(buildingCode);
      expect(floorRepoMock.findByCode).toBeCalledWith(buildingCode, floorNumber);
      expect(floorRepoMock.save).toBeCalledWith(expect.any(Object));
    });

    it('should fail to load a map when building with the provided code does not exist', async () => {
      const buildingCode = 'B2';
      const floorNumber = 1;
      const mapData = {
        size: { width: 2, height: 2 },
        map: [
          [1, 2],
          [3, 4],
        ],
      };

      buildingRepoMock.findByCode.mockResolvedValue(null as any);

      const result = await floorService.loadMap(buildingCode, floorNumber, mapData);

      expect(result.isFailure).toBe(true);
      expect(result.error).toBe('Building with the provided code does not exist.');
      expect(buildingRepoMock.findByCode).toBeCalledWith(buildingCode);
      expect(floorRepoMock.findByCode).not.toBeCalled();
      expect(floorRepoMock.save).not.toBeCalled();
    });

    it('should fail to load a map when map size is bigger than the maximum floor size of the building', async () => {
      const buildingCode = 'B1';
      const floorNumber = 1;
      const mapData = {
        size: { width: 3, height: 3 }, // Map size exceeds maximum floor size
        map: [
          [1, 2, 3],
          [4, 5, 6],
          [7, 8, 9],
        ],
      };

      const buildingMock = {
        code: { value: buildingCode },
        floorSize: { value: { width: 2, height: 2 } },
      };

      buildingRepoMock.findByCode.mockResolvedValue(buildingMock as any);

      const result = await floorService.loadMap(buildingCode, floorNumber, mapData);

      expect(result.isFailure).toBe(true);
      expect(result.error).toBe('Map size is bigger than the maximum floor size of the building.');
      expect(buildingRepoMock.findByCode).toBeCalledWith(buildingCode);
      expect(floorRepoMock.findByCode).not.toBeCalled();
      expect(floorRepoMock.save).not.toBeCalled();
    });
  });
});
