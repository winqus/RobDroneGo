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

  const buildingRepo = {
    name: config.repos.building.name,
    path: config.repos.building.path,
  };

  const floorRepo = {
    name: config.repos.floor.name,
    path: config.repos.floor.path,
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

  await dependencyInjectorLoader({
    mongoConnection,
    schemas: [userSchema, roleSchema, buildingSchema, floorSchema, passageSchema],
    controllers: [roleController, buildingController, floorController, passageController],
    repos: [roleRepo, userRepo, buildingRepo, floorRepo, passageRepo],
    services: [roleService, buildingService, floorService, passageService],
  });
  Logger.info('✌️ Schemas, Controllers, Repositories, Services, etc. loaded');

  await expressLoader({ app: expressApp });
  Logger.info('✌️ Express loaded');
};
