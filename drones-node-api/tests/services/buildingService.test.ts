import exp from 'constants';
import { MockProxy, mock } from 'jest-mock-extended';
import { UniqueEntityID } from '../../src/core/domain/UniqueEntityID';
import { Elevator } from '../../src/domain/Building/Entities/elevator';
import { Building } from '../../src/domain/Building/building';
import IBuildingDTO from '../../src/dto/IBuildingDTO';
import IElevatorDTO from '../../src/dto/IElevatorDTO';
import { BuildingMap } from '../../src/mappers/BuildingMap';
import { ElevatorMap } from '../../src/mappers/ElevatorMap';
import IBuildingRepo from '../../src/services/IRepos/IBuildingRepo';
import IFloorRepo from '../../src/services/IRepos/IFloorRepo';
import BuildingService from '../../src/services/buildingService';

describe('BuildingService', () => {
  let buildingService: BuildingService;
  let buildingRepoMock: MockProxy<IBuildingRepo>;
  let floorRepoMock: MockProxy<IFloorRepo>;
  let buildingStub: Building;
  let buildingDTOstub: IBuildingDTO;
  let elevatorStub: Elevator;
  let elevatorDTOstub: IElevatorDTO;

  beforeEach(() => {
    buildingRepoMock = mock<IBuildingRepo>();
    floorRepoMock = mock<IFloorRepo>();

    buildingStub = {
      id: new UniqueEntityID(),
      name: { value: 'Building1' },
      code: { value: 'A1' },
      description: { value: 'Test building' },
      floorSize: { value: { length: 100, width: 200 } },
    } as Building;
    buildingDTOstub = BuildingMap.toDTO(buildingStub) as IBuildingDTO;

    elevatorDTOstub = {
      id: '123',
      number: 1,
      make: 'Make',
      model: 'Model',
      serialNumber: 'SerialNumber',
      description: 'Description',
    } as IElevatorDTO;
    elevatorStub = ElevatorMap.toDomain(elevatorDTOstub) as Elevator;

    buildingService = new BuildingService(buildingRepoMock, floorRepoMock);
  });

  describe('createBuilding', () => {
    it('should successfully create a building', async () => {
      const buildingDTO: IBuildingDTO = {
        id: '00000000-0000-0000-0000-000000000000',
        name: 'Building1',
        code: 'A1',
        description: 'Test building',
        floorSizeLength: 100,
        floorSizeWidth: 200,
      };

      buildingRepoMock.save.mockResolvedValue(buildingStub as any);
      buildingRepoMock.findByCode.mockResolvedValue(null as any);

      const result = await buildingService.createBuilding(buildingDTO);

      expect(result.isSuccess).toBe(true);
      expect(buildingRepoMock.save).toBeCalled();
    });

    it('should fail to create a building when input is invalid', async () => {
      const buildingDTO: IBuildingDTO = {
        id: '00000000-0000-0000-0000-000000000000',
        name: '---invalid name---',
        code: 'A1',
        description: 'Test building',
        floorSizeLength: 100,
        floorSizeWidth: 200,
      };

      const result = await buildingService.createBuilding(buildingDTO);

      expect(result.isFailure).toBe(true);
      expect(buildingRepoMock.save).not.toBeCalled();
    });
  });

  describe('updateBuilding', () => {
    it('should successfully update a building', async () => {
      const buildingDTO: IBuildingDTO = {
        id: '00000000-0000-0000-0000-000000000000',
        name: 'Building1',
        code: 'A1',
        description: 'Test building',
        floorSizeLength: 100,
        floorSizeWidth: 200,
      };

      buildingRepoMock.findById.mockResolvedValue(buildingStub as any);
      buildingRepoMock.save.mockResolvedValue(buildingStub as any);

      const result = await buildingService.updateBuilding(buildingDTO);

      expect(result.isSuccess).toBe(true);
      expect(buildingRepoMock.findById).toHaveBeenCalledWith(buildingDTO.id);
      expect(buildingRepoMock.save).toBeCalled();
    });

    it('should fail to update a building when input is invalid', async () => {
      const buildingDTO: IBuildingDTO = {
        id: '00000000-0000-0000-0000-000000000000',
        name: '---invalid name---',
        code: 'A1',
        description: 'Test building',
        floorSizeLength: 100,
        floorSizeWidth: 200,
      };

      buildingRepoMock.findById.mockResolvedValue(buildingStub as any);

      const result = await buildingService.updateBuilding(buildingDTO);

      expect(result.isFailure).toBe(true);
      expect(buildingRepoMock.findById).toHaveBeenCalledWith(buildingDTO.id);
      expect(buildingRepoMock.save).not.toBeCalled();
    });

    it('should successfully update a building when only floor width is provided', async () => {
      const buildingDTO: any = {
        id: '00000000-0000-0000-0000-000000000000',
        floorSizeWidth: 201,
      };
      const expectedBuildingStub = {
        ...buildingStub,
        floorSize: {
          props: {
            length: buildingStub.floorSize.value.length,
            width: buildingDTO.floorSizeWidth,
          },
        },
      } as Building;

      buildingRepoMock.findById.mockResolvedValue(buildingStub as any);
      buildingRepoMock.save.mockResolvedValue(buildingStub as any);

      const result = await buildingService.updateBuilding(buildingDTO);

      expect(result.isSuccess).toBe(true);
      expect(buildingRepoMock.findById).toHaveBeenCalledWith(buildingDTO.id);
      expect(buildingRepoMock.save).toHaveBeenCalledWith(expectedBuildingStub);
    });

    it('should successfully update a building when only floor length is provided', async () => {
      const buildingDTO: any = {
        id: '00000000-0000-0000-0000-000000000000',
        floorSizeLength: 101,
      };
      const expectedBuildingStub = {
        ...buildingStub,
        floorSize: {
          props: {
            length: buildingDTO.floorSizeLength,
            width: buildingStub.floorSize.value.width,
          },
        },
      } as Building;

      buildingRepoMock.findById.mockResolvedValue(buildingStub as any);
      buildingRepoMock.save.mockResolvedValue(buildingStub as any);

      const result = await buildingService.updateBuilding(buildingDTO);

      expect(result.isSuccess).toBe(true);
      expect(buildingRepoMock.findById).toHaveBeenCalledWith(buildingDTO.id);
      expect(buildingRepoMock.save).toHaveBeenCalledWith(expectedBuildingStub);
    });
  });

  describe('getAllBuildings', () => {
    it('should successfully get all buildings', async () => {
      buildingRepoMock.findAllBuildings.mockResolvedValue([buildingStub] as any);

      const result = await buildingService.getAllBuildings();

      expect(result.isSuccess).toBe(true);
      expect(buildingRepoMock.findAllBuildings).toBeCalled();
    });

    it('should return empty list when there are no buildings', async () => {
      buildingRepoMock.findAllBuildings.mockResolvedValue([] as any);

      const result = await buildingService.getAllBuildings();

      expect(result.isSuccess).toBe(true);
      expect(result.getValue()).toEqual([]);
      expect(buildingRepoMock.findAllBuildings).toBeCalled();
    });
  });

  describe('listElevatorsInBuilding', () => {
    it('should successfully list elevators in a building', async () => {
      buildingRepoMock.findByCode.mockResolvedValue(buildingStub as any);
      buildingRepoMock.findElevatorsInBuilding.mockResolvedValue([elevatorStub] as any);

      const result = await buildingService.listElevatorsInBuilding(buildingDTOstub.id);

      expect(result.isSuccess).toBe(true);
      expect(result.getValue()).toEqual([elevatorDTOstub]);
    });
  });
});
