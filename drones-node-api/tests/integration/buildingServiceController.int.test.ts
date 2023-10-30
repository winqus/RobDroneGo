import BuildingController from '../../src/controllers/buildingController';
import FakeBuildingRepo from '../../src/repos/Fake/fakeBuildingRepo';
import IBuildingRepo from '../../src/services/IRepos/IBuildingRepo';
import IBuildingService from '../../src/services/IServices/IBuildingService';
import BuildingService from '../../src/services/buildingService';
import { mockNext, mockRequest, mockResponse } from '../../src/utils/controllerInterceptor';

describe('BuildingService and BuildingController Tests', () => {
  let buildingService: BuildingService;
  let buildingController: BuildingController;
  let fakeBuildingRepo;
  let fakeFloorRepo;
  let fakeReq: any;
  let fakeRes: any;
  let fakeNext: any;
  let buildingRawGood: any;
  let buildingRawBad: any;

  beforeEach(() => {
    buildingRawGood = {
      name: 'TestBuilding',
      code: 'TB123',
      description: 'A building for integration testing',
      floorSizeLength: 100,
      floorSizeWidth: 50,
    };
    buildingRawBad = {
      name: '!@#',
      code: 'TB12312312323',
    };

    fakeBuildingRepo = new FakeBuildingRepo();
    fakeFloorRepo = {} as any;
    buildingService = new BuildingService(fakeBuildingRepo as IBuildingRepo, fakeFloorRepo);
    buildingController = new BuildingController(buildingService as IBuildingService);

    fakeReq = mockRequest();
    fakeRes = mockResponse();
    fakeNext = mockNext();
  });

  it('should create and save a building with correct data', async () => {
    fakeReq.body = buildingRawGood;

    await buildingController.createBuilding(fakeReq, fakeRes, fakeNext);
    const building = fakeBuildingRepo.getItems()[0];

    expect(fakeRes.status).toHaveBeenCalledWith(201);
    expect(fakeRes.json).toHaveBeenCalled();
    expect(building).toBeDefined();
    expect(building.name.value).toBe(buildingRawGood.name);
  });

  it('should fail to create a new building when input is incorrect', async () => {
    fakeReq.body = buildingRawBad;

    await buildingController.createBuilding(fakeReq, fakeRes, fakeNext);

    expect(fakeRes.status).toHaveBeenCalledWith(400);
    expect(fakeRes.json).toHaveBeenCalled();
  });

  it('should fail to create buildings with same code', async () => {
    fakeReq.body = buildingRawGood;

    await buildingController.createBuilding(fakeReq, fakeRes, fakeNext);
    await buildingController.createBuilding(fakeReq, fakeRes, fakeNext);

    expect(fakeRes.status).toHaveBeenCalledWith(400);
    expect(fakeRes.json).toHaveBeenCalled();
  });

  it('should handle errors gracefully', async () => {
    fakeReq.body = {} as any;

    await buildingController.createBuilding(fakeReq, fakeRes, fakeNext);

    expect(fakeRes.status).toHaveBeenCalledWith(400);
    expect(fakeRes.json).toHaveBeenCalled();
  });
});
