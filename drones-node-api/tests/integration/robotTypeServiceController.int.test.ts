import RobotTypeController from '../../src/controllers/robotTypeController';
import FakeRobotTypeRepo from '../../src/repos/Fake/fakeRobotTypeRepo';
import FakeTaskTypeRepo from '../../src/repos/Fake/fakeTaskTypeRepo';
import IRobotTypeRepo from '../../src/services/IRepos/IRobotTypeRepo';
import ITaskTypeRepo from '../../src/services/IRepos/ITaskTypeRepo';
import IRobotTypeService from '../../src/services/IServices/IRobotTypeService';
import ITaskTypeService from '../../src/services/IServices/ITaskTypeService';
import RobotTypeService from '../../src/services/robotTypeService';
import TaskTypeService from '../../src/services/taskTypeService';
import { mockNext, mockRequest, mockResponse } from '../../src/utils/controllerInterceptor';

describe('RobotTypeService and RobotTypeController Tests', () => {
  let robotTypeService: RobotTypeService;
  let taskTypeService: TaskTypeService;
  let robotTypeController: RobotTypeController;
  let fakeRobotTypeRepo;
  let fakeTaskTypeRepo;
  let fakeReq: any;
  let fakeRes: any;
  let fakeNext: any;
  let robotTypeRawGood: any;
  let robotTypeRawBad: any;

  beforeEach(() => {
    robotTypeRawGood = {
      name: 'X',
      brand: 'A',
      model: 'A',
      typesOfTasks: ['PickUpAndDelivery', 'Surveillance'],
    };
    robotTypeRawBad = {
      name: 'X',
      brand: 'A',
      model: null,
      typesOfTasks: ['PickUpAndDelivery', 'Surveillance'],
    };

    fakeRobotTypeRepo = new FakeRobotTypeRepo();
    fakeTaskTypeRepo = new FakeTaskTypeRepo();
    taskTypeService = new TaskTypeService(fakeTaskTypeRepo as ITaskTypeRepo);
    robotTypeService = new RobotTypeService(fakeRobotTypeRepo as IRobotTypeRepo, taskTypeService as ITaskTypeService);
    robotTypeController = new RobotTypeController(robotTypeService as IRobotTypeService);

    fakeReq = mockRequest();
    fakeRes = mockResponse();
    fakeNext = mockNext();
  });

  it('should create and save a robotType with correct data', async () => {
    fakeReq.body = robotTypeRawGood;

    await robotTypeController.createRobotType(fakeReq, fakeRes, fakeNext);
    const robotType = fakeRobotTypeRepo.getItems()[0];

    expect(fakeRes.status).toHaveBeenCalledWith(201);
    expect(fakeRes.json).toHaveBeenCalled();
    expect(robotType).toBeDefined();
    expect(robotType.name.value).toBe(robotTypeRawGood.name);
  });

  it('should fail to create a new robotType when input is incorrect', async () => {
    fakeReq.body = robotTypeRawBad;

    await robotTypeController.createRobotType(fakeReq, fakeRes, fakeNext);

    expect(fakeRes.status).toHaveBeenCalledWith(400);
    expect(fakeRes.json).toHaveBeenCalled();
  });
  it('should fail to create robotTypes with same code', async () => {
    fakeReq.body = robotTypeRawGood;

    await robotTypeController.createRobotType(fakeReq, fakeRes, fakeNext);
    await robotTypeController.createRobotType(fakeReq, fakeRes, fakeNext);

    expect(fakeRes.status).toHaveBeenCalledWith(400);
    expect(fakeRes.json).toHaveBeenCalled();
  });
});
