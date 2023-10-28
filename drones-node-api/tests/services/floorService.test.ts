import { isNull } from 'lodash';
import { Container } from 'typedi';
import { UniqueEntityID } from '../../src/core/domain/UniqueEntityID';
import { Result } from '../../src/core/logic/Result';
import { Floor } from '../../src/domain/Floor/floor';
import IFloorDTO from '../../src/dto/IFloorDTO';
import IFloorRepo from '../../src/services/IRepos/IFloorRepo';
import FloorService from '../../src/services/floorService';

describe('FloorService', () => {
  let floorService: FloorService;
  let floorRepoMock: jest.Mocked<IFloorRepo>;
  let buildingServiceMock: jest.Mocked<any>;
  let floorStub: Floor;

  beforeEach(() => {
    floorRepoMock = {
      save: jest.fn(),
      exists: jest.fn(),
    };

    buildingServiceMock = {
      createBuilding: jest.fn(),
      updateBuilding: jest.fn(),
      getBuildingByCode: jest.fn(),
      getAllBuildings: jest.fn(),
    };

    floorStub = {
      id: new UniqueEntityID(),
      floorNumber: 1,
      description: { value: 'Test floor' },
      servedByElevator: true,
      buildingCode: { value: 'B1' },
    } as Floor;

    Container.set('floorRepo', floorRepoMock);
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
      buildingServiceMock.getBuildingByCode.mockResolvedValue(Result.ok(true));

      const result = await floorService.createFloor(floorDTO);

      expect(result.isSuccess).toBe(true);
      expect(floorRepoMock.save).toBeCalled();
    });

    it('should fail to create a floor when input is invalid', async () => {
      const floorDTO: IFloorDTO = {
        id: '00000000-0000-0000-0000-000000000000',
        floorNumber: null,
        description: 'Test floor',
        servedByElevator: true,
        buildingCode: 'B1',
      };

      const result = await floorService.createFloor(floorDTO);

      expect(result.isFailure).toBe(true);
      expect(floorRepoMock.save).not.toBeCalled();
    });
  });
});
