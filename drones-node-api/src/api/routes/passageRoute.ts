import { celebrate, errors, Joi } from 'celebrate';
import { Router } from 'express';
import { Container } from 'typedi';
import config from '../../../config';
import IPassageController from '../../controllers/IControllers/IPassageController';
import { UserRole } from '../../domain/userRole.enum';
import middlewares from '../middlewares';
import routeJoiErrorHandler from '../middlewares/routeJoiErrorHandler';

const route = Router();
const protectedRoute = Router();

protectedRoute.use(middlewares.isAuth);
protectedRoute.use(middlewares.attachCurrentUser);
protectedRoute.use(middlewares.requireAnyRole([UserRole.CampusManager]));

export default (app: Router) => {
  app.use('/passage', route);
  app.use('/passage', protectedRoute);

  const controller = Container.get(config.controllers.passage.name) as IPassageController;

  protectedRoute.post(
    '',
    celebrate({
      body: Joi.object({
        buildingCode1: Joi.string().required(),
        buildingCode2: Joi.string().required(),
        floorNumber1: Joi.number()
          .integer()
          .required(),
        floorNumber2: Joi.number()
          .integer()
          .required(),
      }),
    }),
    (req, res, next) => controller.createPassage(req, res, next),
    errors(),
    routeJoiErrorHandler,
  );

  route.get(
    '/',
    celebrate({
      query: Joi.object({
        buildingCode1: Joi.string(),
        buildingCode2: Joi.string(),
      }).and('buildingCode1', 'buildingCode2'),
    }),
    (req, res, next) => controller.getPassages(req, res, next),
    errors(),
    routeJoiErrorHandler,
  );

  route.get(
    '/toDifferentBuildings',
    celebrate({
      query: Joi.object({
        buildingCode: Joi.string().required(),
      }),
    }),
    (req, res, next) => controller.listFloorsWithPassagesToDifferentBuilding(req, res, next),
    errors(),
    routeJoiErrorHandler,
  );

  protectedRoute.put(
    '/',
    celebrate({
      body: Joi.object({
        oldPassage: Joi.object({
          buildingCode1: Joi.string().required(),
          buildingCode2: Joi.string().required(),
          floorNumber1: Joi.number()
            .integer()
            .required(),
          floorNumber2: Joi.number()
            .integer()
            .required(),
        }),
        newPassage: Joi.object({
          buildingCode1: Joi.string().required(),
          buildingCode2: Joi.string().required(),
          floorNumber1: Joi.number()
            .integer()
            .required(),
          floorNumber2: Joi.number()
            .integer()
            .required(),
        }),
      }),
    }),
    (req, res, next) => controller.updatePassage(req, res, next),
    errors(),
    routeJoiErrorHandler,
  );
};
