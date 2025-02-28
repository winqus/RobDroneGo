import dependencyInjectorLoader from './dependencyInjector';
import expressLoader from './express';
import Logger from './logger';
import mongooseLoader from './mongoose';

import config from '../../config';

export default async ({ expressApp }) => {
  const mongoConnection = await mongooseLoader();
  Logger.info('✌️ DB loaded and connected!');

  const userSchema = {
    // compare with the approach followed in repos and services
    name: 'userSchema',
    schema: '../persistence/schemas/userSchema',
  };

  const roleSchema = {
    // compare with the approach followed in repos and services
    name: 'roleSchema',
    schema: '../persistence/schemas/roleSchema',
  };

  const buildingSchema = {
    name: 'buildingSchema',
    schema: '../persistence/schemas/buildingSchema',
  };

  const floorSchema = {
    name: 'floorSchema',
    schema: '../persistence/schemas/floorSchema',
  };

  const passageSchema = {
    name: 'passageSchema',
    schema: '../persistence/schemas/passageSchema',
  };

  const robotTypeSchema = {
    name: 'robotTypeSchema',
    schema: '../persistence/schemas/robotTypeSchema',
  };

  const taskTypeSchema = {
    name: 'taskTypeSchema',
    schema: '../persistence/schemas/taskTypeSchema',
  };

  const roomSchema = {
    name: 'roomSchema',
    schema: '../persistence/schemas/roomSchema',
  };

  const taskRequestSchema = {
    name: 'taskRequestSchema',
    schema: '../persistence/schemas/taskRequestSchema',
  };

  const roomService = {
    name: config.services.room.name,
    path: config.services.room.path,
  };

  const roomRepo = {
    name: config.repos.room.name,
    path: config.repos.room.path,
  };

  const roomController = {
    name: config.controllers.room.name,
    path: config.controllers.room.path,
  };

  const robotSchema = {
    name: 'robotSchema',
    schema: '../persistence/schemas/robotSchema',
  };

  const roleController = {
    name: config.controllers.role.name,
    path: config.controllers.role.path,
  };

  const roleRepo = {
    name: config.repos.role.name,
    path: config.repos.role.path,
  };

  const userRepo = {
    name: config.repos.user.name,
    path: config.repos.user.path,
  };

  const userService = {
    name: config.services.user.name,
    path: config.services.user.path,
  };

  const userController = {
    name: config.controllers.user.name,
    path: config.controllers.user.path,
  };

  const roleService = {
    name: config.services.role.name,
    path: config.services.role.path,
  };

  const buildingController = {
    name: config.controllers.building.name,
    path: config.controllers.building.path,
  };

  const floorController = {
    name: config.controllers.floor.name,
    path: config.controllers.floor.path,
  };

  const robotTypeController = {
    name: config.controllers.robotType.name,
    path: config.controllers.robotType.path,
  };

  const buildingRepo = {
    name: config.repos.building.name,
    path: config.repos.building.path,
  };

  const floorRepo = {
    name: config.repos.floor.name,
    path: config.repos.floor.path,
  };

  const robotTypeRepo = {
    name: config.repos.robotType.name,
    path: config.repos.robotType.path,
  };

  const taskTypeRepo = {
    name: config.repos.taskType.name,
    path: config.repos.taskType.path,
  };

  const buildingService = {
    name: config.services.building.name,
    path: config.services.building.path,
  };

  const floorService = {
    name: config.services.floor.name,
    path: config.services.floor.path,
  };

  const passageController = {
    name: config.controllers.passage.name,
    path: config.controllers.passage.path,
  };

  const passageRepo = {
    name: config.repos.passage.name,
    path: config.repos.passage.path,
  };

  const passageService = {
    name: config.services.passage.name,
    path: config.services.passage.path,
  };

  const robotTypeService = {
    name: config.services.robotType.name,
    path: config.services.robotType.path,
  };

  const taskTypeService = {
    name: config.services.taskType.name,
    path: config.services.taskType.path,
  };

  const elevatorService = {
    name: config.services.elevator.name,
    path: config.services.elevator.path,
  };

  const robotController = {
    name: config.controllers.robot.name,
    path: config.controllers.robot.path,
  };

  const robotRepo = {
    name: config.repos.robot.name,
    path: config.repos.robot.path,
  };

  const robotService = {
    name: config.services.robot.name,
    path: config.services.robot.path,
  };

  const fileController = {
    name: config.controllers.file.name,
    path: config.controllers.file.path,
  };

  const fileService = {
    name: config.services.file.name,
    path: config.services.file.path,
  };

  const fileRepo = {
    name: config.repos.file.name,
    path: config.repos.file.path,
  };

  const planningService = {
    name: config.services.planning.name,
    path: config.services.planning.path,
  };

  const planningController = {
    name: config.controllers.planning.name,
    path: config.controllers.planning.path,
  };

  const taskRequestService = {
    name: config.services.taskRequest.name,
    path: config.services.taskRequest.path,
  };

  const taskRequestRepo = {
    name: config.repos.taskRequest.name,
    path: config.repos.taskRequest.path,
  };

  const taskRequestController = {
    name: config.controllers.taskRequest.name,
    path: config.controllers.taskRequest.path,
  };

  await dependencyInjectorLoader({
    mongoConnection,
    schemas: [
      userSchema,
      roleSchema,
      buildingSchema,
      floorSchema,
      passageSchema,
      taskTypeSchema,
      robotTypeSchema,
      robotSchema,
      roomSchema,
      taskRequestSchema,
    ],
    controllers: [
      roleController,
      buildingController,
      floorController,
      passageController,
      robotTypeController,
      robotController,
      roomController,
      planningController,
      fileController,
      userController,
      taskRequestController,
    ],
    repos: [
      roleRepo,
      userRepo,
      buildingRepo,
      floorRepo,
      passageRepo,
      taskTypeRepo,
      robotTypeRepo,
      robotRepo,
      roomRepo,
      fileRepo,
      taskRequestRepo,
    ],
    services: [
      roleService,
      buildingService,
      floorService,
      passageService,
      taskTypeService,
      robotTypeService,
      elevatorService,
      roomService,
      robotService,
      planningService,
      fileService,
      userService,
      taskRequestService,
    ],
  });
  Logger.info('✌️ Schemas, Controllers, Repositories, Services, etc. loaded');

  await expressLoader({ app: expressApp });
  Logger.info('✌️ Express loaded');
};
