import BuildingController from '../../src/controllers/buildingController';
import { Code as BuildingCode } from '../../src/domain/Building/ValueObjects/code';
import { Description } from '../../src/domain/Building/ValueObjects/description';
import { Floor } from '../../src/domain/Floor/floor';
import FakeBuildingRepo from '../../src/repos/Fake/fakeBuildingRepo';
import FakeFloorRepo from '../../src/repos/Fake/fakeFloorRepo';
import IBuildingRepo from '../../src/services/IRepos/IBuildingRepo';
import IFloorRepo from '../../src/services/IRepos/IFloorRepo';
import BuildingService from '../../src/services/buildingService';
import ElevatorService from '../../src/services/elevatorService';
import { mockNext, mockRequest, mockResponse } from '../../src/utils/controllerInterceptor';

describe('BuildingService and BuildingController Tests', () => {
  let buildingService: BuildingService;
  let elevatorService: ElevatorService;
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
      elevator: null,
    };
    buildingRawBad = {
      name: '!@#',
      code: 'TB12312312323',
    };

    fakeBuildingRepo = new FakeBuildingRepo();
    fakeFloorRepo = new FakeFloorRepo();
    buildingService = new BuildingService(fakeBuildingRepo as IBuildingRepo, fakeFloorRepo as IFloorRepo);
    elevatorService = new ElevatorService(fakeBuildingRepo as IBuildingRepo, fakeFloorRepo as IFloorRepo);
    buildingController = new BuildingController(buildingService, elevatorService);

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

  it('should create and save a building with correct data', async () => {
    fakeReq.body = buildingRawGood;

    await buildingController.createBuilding(fakeReq, fakeRes, fakeNext);
    fakeFloorRepo.save(
      Floor.create({
        buildingCode: BuildingCode.create('TB123').getValue(),
        floorNumber: 4,
        description: Description.create('floor 4').getValue(),
        servedByElevator: false,
      }).getValue(),
    );
    fakeFloorRepo.save(
      Floor.create({
        buildingCode: BuildingCode.create('TB123').getValue(),
        floorNumber: 5,
        description: Description.create('floor 5').getValue(),
        servedByElevator: false,
      }).getValue(),
    );

    fakeReq = mockRequest();
    fakeRes = mockResponse();
    fakeNext = mockNext();

    fakeReq.body = {
      number: 1,
      make: 'brand X',
      model: 'model Y',
      serialNumber: '00001',
      description: 'elevator of building TB123',
      floors: [4, 5],
    };
    fakeReq.params = {
      code: 'TB123',
    };

    await buildingController.createElevator(fakeReq, fakeRes, fakeNext);

    expect(fakeRes.status).toHaveBeenCalledWith(201);
    expect(fakeRes.json).toHaveBeenCalled();
    expect(fakeFloorRepo.getItems()[0].servedByElevator).toBe(true);
    expect(fakeFloorRepo.getItems()[1].servedByElevator).toBe(true);
  });

  it('should fail to create and save in building with one elevator already', async () => {
    fakeReq.body = buildingRawGood;

    await buildingController.createBuilding(fakeReq, fakeRes, fakeNext);
    fakeFloorRepo.save(
      Floor.create({
        buildingCode: BuildingCode.create('TB123').getValue(),
        floorNumber: 4,
        description: Description.create('floor 4').getValue(),
        servedByElevator: false,
      }).getValue(),
    );
    fakeFloorRepo.save(
      Floor.create({
        buildingCode: BuildingCode.create('TB123').getValue(),
        floorNumber: 5,
        description: Description.create('floor 5').getValue(),
        servedByElevator: false,
      }).getValue(),
    );

    fakeReq = mockRequest();
    fakeRes = mockResponse();
    fakeNext = mockNext();

    fakeReq.body = {
      number: 1,
      make: 'brand X',
      model: 'model Y',
      serialNumber: '00001',
      description: 'elevator of building TB123',
      floors: [4, 5],
    };
    fakeReq.params = {
      code: 'TB123',
    };

    await buildingController.createElevator(fakeReq, fakeRes, fakeNext);
    await buildingController.createElevator(fakeReq, fakeRes, fakeNext);

    expect(fakeRes.status).toHaveBeenCalledWith(400);
    expect(fakeRes.json).toHaveBeenCalled();
  });
});
