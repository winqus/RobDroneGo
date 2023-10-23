import { Container } from 'typedi';
import { UniqueEntityID } from '../../src/core/domain/UniqueEntityID';
import { Building } from '../../src/domain/Building/building';
import IBuildingDTO from '../../src/dto/IBuildingDTO';
import IBuildingRepo from '../../src/services/IRepos/IBuildingRepo';
import BuildingService from '../../src/services/buildingService';

describe('BuildingService', () => {
  let buildingService: BuildingService;
  let buildingRepoMock: jest.Mocked<IBuildingRepo>;
  let buildingStub: Building;

  beforeEach(() => {
    buildingRepoMock = {
      save: jest.fn(),
      exists: jest.fn(),
    };

    buildingStub = {
      id: new UniqueEntityID(),
      name: { value: 'Building1' },
      code: { value: 'A1' },
      description: { value: 'Test building' },
      floorSize: { value: { length: 100, width: 200 } },
    } as Building;

    Container.set('buildingRepo', buildingRepoMock);
    buildingService = new BuildingService(buildingRepoMock);
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
});
