import http from 'http';
import { ParsedQs } from 'qs';
import { Inject, Service } from 'typedi';
import config from '../../config';
import { Result } from '../core/logic/Result';
import { DeliveryTask as Delivery } from '../domain/DeliveryTask/deliveryTask';
import { Robot } from '../domain/Robot/robot';
import { SurveillanceTask } from '../domain/SurveillanceTask/surveillanceTask';
import { TaskRequest } from '../domain/TaskRequest/taskRequest';
import { TaskStatus } from '../domain/TaskRequest/taskStatus';
import { Types } from '../domain/TaskType/type';
import { DeliveryTask } from '../persistence/schemas/deliveryTaskSchema';
import IFloorRepo from './IRepos/IFloorRepo';
import IRobotRepo from './IRepos/IRobotRepo';
import IRobotTypeRepo from './IRepos/IRobotTypeRepo';
import IRoomRepo from './IRepos/IRoomRepo';
import ITaskRequestRepo from './IRepos/ITaskRequestRepo';
import IPlanningService from './IServices/IPlanningService';

@Service()
export default class PlanningService implements IPlanningService {
  constructor(
    @Inject(config.repos.floor.name) private floorRepo: IFloorRepo,
    @Inject(config.repos.room.name) private roomRepo: IRoomRepo,
    @Inject(config.repos.taskRequest.name) private taskRequestRepo: ITaskRequestRepo,
    @Inject(config.repos.robot.name) private robotRepo: IRobotRepo,
    @Inject(config.repos.robotType.name) private robotTypeRepo: IRobotTypeRepo,
  ) {}

  public async calculateCells(query: ParsedQs): Promise<Result<any>> {
    const originRoom = await this.roomRepo.findById(query.origin_room as string);
    const destinationRoom = await this.roomRepo.findById(query.destination_room as string);

    if (!originRoom) {
      return Result.fail<any>('Origin room not found');
    }

    if (!destinationRoom) {
      return Result.fail<any>('Destination room not found');
    }

    const originFloor = await this.floorRepo.getFloorOfRoom(originRoom.name.value as string);
    const destinationFloor = await this.floorRepo.getFloorOfRoom(destinationRoom.name.value as string);

    const originRoomPositionX = originRoom.position.props.x;
    const originRoomPositionY = originRoom.position.props.y;
    const originRoomWidth = originRoom.size.props.width;
    const originRoomLength = originRoom.size.props.length;

    const destinationRoomPositionX = destinationRoom.position.props.x;
    const destinationRoomPositionY = destinationRoom.position.props.y;
    const destinationRoomWidth = destinationRoom.size.props.width;
    const destinationRoomLength = destinationRoom.size.props.length;

    let origin_map_cell_x = null;
    let origin_map_cell_y = null;
    let destination_map_cell_x = null;
    let destination_map_cell_y = null;

    for (let y = originRoomPositionY; y <= originRoomPositionY + originRoomLength; y++) {
      for (let x = originRoomPositionX; x <= originRoomPositionX + originRoomWidth; x++) {
        if (originFloor.map.map[y][x] === 4 || originFloor.map.map[y][x] === 5) {
          origin_map_cell_x = x;
          origin_map_cell_y = y;
        }
      }
    }

    for (let y = destinationRoomPositionY; y <= destinationRoomPositionY + destinationRoomLength; y++) {
      for (let x = destinationRoomPositionX; x <= destinationRoomPositionX + destinationRoomWidth; x++) {
        if (destinationFloor.map.map[y][x] === 4 || destinationFloor.map.map[y][x] === 5) {
          destination_map_cell_x = x;
          destination_map_cell_y = y;
        }
      }
    }

    if (
      origin_map_cell_x !== null &&
      origin_map_cell_y !== null &&
      destination_map_cell_x !== null &&
      destination_map_cell_y !== null
    ) {
      const result = {
        origin_map_cell_x: origin_map_cell_x,
        origin_map_cell_y: origin_map_cell_y,
        destination_map_cell_x: destination_map_cell_x,
        destination_map_cell_y: destination_map_cell_y,
      };

      return Result.ok<any>(result);
    } else {
      return Result.fail<any>('No room door found');
    }
  }

  public async planTasks(tasks: string[]): Promise<Result<any>> {
    try {
      const task = [];
      for (let i = 0; i < tasks.length; i++) {
        task[i] = await this.taskRequestRepo.getById(tasks[i]);
        if (!(task[i].status === TaskStatus.Approved || task[i].status === TaskStatus.Planned)) {
          return Result.fail<any>('One or more tasks are not in the correct state');
        }
        if (task[i].task instanceof Delivery && task[i].navigationData == null) {
          return Result.fail<any>('One or more delivery tasks do not have navigation data');
        }
      }
      const robots = await this.robotRepo.findAvailable();
      if (robots.length === 0) {
        return Result.fail<any>('No robots available');
      }
      const deliveryRobots = [];
      const surveillanceRobots = [];
      const mixedRobots = [];

      for (let i = 0; i < robots.length; i++) {
        const robotType = await this.robotTypeRepo.findByName(robots[i].type.value);

        if (robotType.typesOfTasks.length > 1) {
          mixedRobots.push(robots[i]);
        } else if (robotType.typesOfTasks[0].type === Types.PickUpAndDelivery) {
          deliveryRobots.push(robots[i]);
        } else if (robotType.typesOfTasks[0].type === Types.Surveillance) {
          surveillanceRobots.push(robots[i]);
        }
      }

      const dRobots = deliveryRobots.concat(mixedRobots);
      const sRobots = surveillanceRobots.concat(mixedRobots.reverse());

      let dRobotCount = 0;
      let sRobotCount = 0;
      let taskQuery = '';
      let robotQuery = '';

      for (let i = 0; i < task.length; i++) {
        const taskT = task[i].task;
        if (taskT instanceof Delivery) {
          if (dRobots.length === 0) {
            return Result.fail<any>('No delivery robots available');
          }

          const robot = dRobots[dRobotCount];
          dRobotCount++;
          if (dRobotCount === dRobots.length) {
            dRobotCount = 0;
          }

          taskQuery = taskQuery.concat(
            'task(' +
              `'${robot.id}'` +
              ', ' +
              `'${task[i].id}'` +
              ', origin(' +
              `'${task[i].navigationData.mapPaths[0].buildingCode}'` +
              ', ' +
              task[i].navigationData.mapPaths[0].floorNumber +
              ', cel(' +
              (task[i].navigationData.mapPaths[0].path[0].col + 1) +
              ', ' +
              (task[i].navigationData.mapPaths[0].path[0].row + 1) +
              ')), destination(' +
              `'${task[i].navigationData.mapPaths[task[i].navigationData.mapPaths.length - 1].buildingCode}'` +
              ', ' +
              task[i].navigationData.mapPaths[task[i].navigationData.mapPaths.length - 1].floorNumber +
              ', cel(' +
              (task[i].navigationData.mapPaths[task[i].navigationData.mapPaths.length - 1].path[
                task[i].navigationData.mapPaths[task[i].navigationData.mapPaths.length - 1].path.length - 1
              ].col +
                1) +
              ', ' +
              (task[i].navigationData.mapPaths[task[i].navigationData.mapPaths.length - 1].path[
                task[i].navigationData.mapPaths[task[i].navigationData.mapPaths.length - 1].path.length - 1
              ].row +
                1) +
              ")), '" +
              Object.keys(Types).at(Object.values(Types).indexOf(Types.PickUpAndDelivery)) +
              "').\n",
          );

          if (!robotQuery.includes(robot.id.toString())) {
            robotQuery = robotQuery.concat(
              'robot(' +
                `'${robot.id}'` +
                ', origin(' +
                `'${robot.position.buildingCode}'` +
                ', ' +
                robot.position.floorNumber +
                ', cel(' +
                (robot.position.cellPosition[0] + 1) +
                ', ' +
                (robot.position.cellPosition[1] + 1) +
                '))).\n',
            );
          }
        } else if (taskT instanceof SurveillanceTask) {
          if (sRobots.length === 0) {
            return Result.fail<any>('No surveillance robots available');
          }

          const robot = sRobots[sRobotCount];
          sRobotCount++;
          if (sRobotCount === sRobots.length) {
            sRobotCount = 0;
          }

          let floor = await this.floorRepo.findByCode(taskT.buildingCode.value, taskT.floorNumber[0]);

          const originRow = Math.floor(floor.map.height / 2) + 1;
          const originCol = Math.floor(floor.map.width / 2) + 1;

          floor = await this.floorRepo.findByCode(
            taskT.buildingCode.value,
            taskT.floorNumber[taskT.floorNumber.length - 1],
          );

          const destinationRow = Math.floor(floor.map.height / 2) + 1;
          const destinationCol = Math.floor(floor.map.width / 2) + 1;

          taskQuery = taskQuery.concat(
            'task(' +
              `'${robot.id}'` +
              ', ' +
              `'${task[i].id}'` +
              ', origin(' +
              `'${taskT.buildingCode.value}'` +
              ', ' +
              taskT.floorNumber[0] +
              ', cel(' +
              originCol +
              ', ' +
              originRow +
              ')), destination(' +
              `'${taskT.buildingCode.value}'` +
              ', ' +
              taskT.floorNumber[taskT.floorNumber.length - 1] +
              ', cel(' +
              (destinationCol + 1) +
              ', ' +
              (destinationRow + 1) +
              ')), ' +
              `'${Object.keys(Types).at(Object.values(Types).indexOf(Types.Surveillance))}'` +
              ').\n',
          );

          if (!robotQuery.includes(robot.id.toString())) {
            robotQuery = robotQuery.concat(
              'robot(' +
                `'${robot.id}'` +
                ', origin(' +
                `'${robot.position.buildingCode}'` +
                ', ' +
                robot.position.floorNumber +
                ', cel(' +
                (robot.position.cellPosition[0] + 1) +
                ', ' +
                (robot.position.cellPosition[1] + 1) +
                '))).\n',
            );
          }
        }
      }
      robotQuery = robotQuery.substring(0, robotQuery.length - 1);

      const query = taskQuery.concat(robotQuery);

      const path = `${config.planningAPI.basePath}/planTasks`;

      const options = {
        hostname: config.planningAPI.hostname,
        port: config.planningAPI.port,
        path: path,
        method: 'POST',
        headers: {
          'Content-Type': 'text/plain',
          'Content-Length': query.length,
        },
      };

      const responseData = await new Promise((resolve, reject) => {
        const req = http.request(options, (res) => {
          let data = '';
          res.on('data', (chunk) => {
            data += chunk;
          });
          res.on('end', () => {
            try {
              const parsedData = JSON.parse(data);
              resolve(parsedData);
            } catch (e) {
              reject(e);
            }
          });
        });

        req.on('error', (error) => {
          console.error(error);
          reject(error);
        });

        req.write(query);
        req.end();
      });

      return Result.ok<any>(responseData);
      // return Result.ok<any>(data);
    } catch (error) {
      return Result.fail<any>(error);
    }
  }
}
