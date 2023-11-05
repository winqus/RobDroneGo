import RobotController from '../../src/controllers/robotController';
import { Code } from '../../src/domain/Robot/ValueObjects/code';
import { Nickname } from '../../src/domain/Robot/ValueObjects/nickname';
import { SerialNumber } from '../../src/domain/Robot/ValueObjects/serialNumber';
import { Robot } from '../../src/domain/Robot/robot';
import { Brand } from '../../src/domain/RobotType/ValueObjects/brand';
import { Model } from '../../src/domain/RobotType/ValueObjects/model';
import { Name } from '../../src/domain/RobotType/ValueObjects/name';
import { RobotType } from '../../src/domain/RobotType/robotType';
import { TaskType } from '../../src/domain/TaskType/taskType';
import { Types } from '../../src/domain/TaskType/type';
import FakeRobotRepo from '../../src/repos/Fake/fakeRobotRepo';
import FakeRobotTypeRepo from '../../src/repos/Fake/fakeRobotTypeRepo';
import IRobotRepo from '../../src/services/IRepos/IRobotRepo';
import IRobotTypeRepo from '../../src/services/IRepos/IRobotTypeRepo';
import IRobotService from '../../src/services/IServices/IRobotService';
import RobotService from '../../src/services/robotService';
import { mockNext, mockRequest, mockResponse } from '../../src/utils/controllerInterceptor';

describe('RobotService and RobotController Tests', () => {
  let robotService: RobotService;
  let robotController: RobotController;
  let fakeRobotRepo;
  let fakeRobotTypeRepo;
  let fakeReq: any;
  let fakeRes: any;
  let fakeNext: any;
  let robotRawGood: any;
  let robotRawBad: any;
  let robotRaw: any;

  beforeEach(() => {
    robotRawGood = {
      code: 'A11',
      description: 'Sample robot',
      nickname: 'Nickname',
      serialNumber: 'A11',
      available: true,
      type: 'Type',
    };
    robotRawBad = {
      code: 'A11',
      description: 'Sample robot',
      nickname: 'Nickname'.repeat(10),
      serialNumber: 'A11',
      available: true,
      type: 'Type',
    };
    robotRaw = {
      code: Code.create('A11' as string).getValue(),
      description: 'Sample robot',
      nickname: Nickname.create('Nickname').getValue(),
      serialNumber: SerialNumber.create('A11' as string).getValue(),
      available: true,
      type: Name.create('Type' as string).getValue(),
    };

    fakeRobotRepo = new FakeRobotRepo();
    fakeRobotTypeRepo = new FakeRobotTypeRepo();
    robotService = new RobotService(fakeRobotRepo as IRobotRepo, fakeRobotTypeRepo as IRobotTypeRepo);
    robotController = new RobotController(robotService as IRobotService);

    fakeReq = mockRequest();
    fakeRes = mockResponse();
    fakeNext = mockNext();
  });

  it('should create and save a robot with correct data', async () => {
    fakeReq.body = robotRawGood;

    // Call fakeRobotTypeRepo to create a robot type

    const rt = RobotType.create({
      name: Name.create('Type' as string).getValue(),
      brand: Brand.create('Brand' as string).getValue(),
      model: Model.create('Model' as string).getValue(),
      typesOfTasks: [TaskType.create({ type: Types.PickUpAndDelivery }).getValue()],
    });

    fakeRobotTypeRepo.save(rt.getValue());
    fakeRobotTypeRepo.findByName(Name.create('Type' as string).getValue());
    await robotController.createRobot(fakeReq, fakeRes, fakeNext);
    const robot = fakeRobotRepo.getItems()[0];

    expect(fakeRes.status).toHaveBeenCalledWith(201);
    expect(fakeRes.json).toHaveBeenCalled();
    expect(robot).toBeDefined();
    expect(robot.code.value).toBe(robotRawGood.code);
  });

  it('should fail to create a new robot when input is incorrect', async () => {
    fakeReq.body = robotRawBad;

    await robotController.createRobot(fakeReq, fakeRes, fakeNext);

    expect(fakeRes.status).toHaveBeenCalledWith(400);
    expect(fakeRes.json).toHaveBeenCalled();
  });

  it('should fail to create robots with same code', async () => {
    fakeReq.body = robotRawGood;

    await robotController.createRobot(fakeReq, fakeRes, fakeNext);
    await robotController.createRobot(fakeReq, fakeRes, fakeNext);

    expect(fakeRes.status).toHaveBeenCalledWith(400);
    expect(fakeRes.json).toHaveBeenCalled();
  });

  it('should fail to change robot status if robot does not exist', async () => {
    fakeReq.body = {
      code: 'A11111',
      available: true,
    };

    await robotController.changeRobotState(fakeReq, fakeRes, fakeNext);

    expect(fakeRes.status).toHaveBeenCalledWith(400);
    expect(fakeRes.json).toHaveBeenCalled();
  });

  it('should successfuly change robot status', async () => {
    fakeReq.body = {
      available: false,
    };
    fakeReq.params.robotCode = 'A11';

    await fakeRobotRepo.save(Robot.create(robotRaw).getValue());

    await robotController.changeRobotState(fakeReq, fakeRes, fakeNext);

    expect(fakeRes.status).toHaveBeenCalledWith(200);
    expect(fakeRes.json).toHaveBeenCalled();
  });
});
