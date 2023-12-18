import { celebrate, errors, Joi } from 'celebrate';
import { Router } from 'express';
import { Container } from 'typedi';
import config from '../../../config';
import IBuildingController from '../../controllers/IControllers/IBuildingController';
import { UserRole } from '../../domain/userRole.enum';
import middlewares from '../middlewares';
import routeJoiErrorHandler from '../middlewares/routeJoiErrorHandler';

const route = Router();
const protectedRoute = Router();

protectedRoute.use(middlewares.isAuth);
protectedRoute.use(middlewares.attachCurrentUser);
protectedRoute.use(middlewares.requireAnyRole([UserRole.CampusManager]));

export default (app: Router) => {
  app.use('/building', route);
  app.use('/building', protectedRoute);

  const controller = Container.get(config.controllers.building.name) as IBuildingController;

  protectedRoute.post(
    '',
    celebrate({
      body: Joi.object({
        name: Joi.string()
          .allow('')
          .optional(),
        code: Joi.string().required(),
        description: Joi.string()
          .allow('')
          .optional(),
        floorSizeLength: Joi.number().required(),
        floorSizeWidth: Joi.number().required(),
      }),
    }),
    (req, res, next) => controller.createBuilding(req, res, next),
    errors(),
    routeJoiErrorHandler,
  );

  protectedRoute.put(
    '/:id',
    celebrate({
      body: Joi.object({
        name: Joi.string()
          .allow('')
          .optional(),
        description: Joi.string()
          .allow('')
          .optional(),
        floorSizeLength: Joi.number().optional(),
        floorSizeWidth: Joi.number().optional(),
      }),
      params: Joi.object({
        id: Joi.string().required(),
      }),
    }),
    (req, res, next) => controller.updateBuilding(req, res, next),
    errors(),
    routeJoiErrorHandler,
  );

  protectedRoute.patch(
    '/:id',
    celebrate({
      body: Joi.object({
        name: Joi.string()
          .allow('')
          .optional(),
        description: Joi.string()
          .allow('')
          .optional(),
        floorSizeLength: Joi.number().optional(),
        floorSizeWidth: Joi.number().optional(),
      }),
      params: Joi.object({
        id: Joi.string().required(),
      }),
    }),
    (req, res, next) => controller.updateBuilding(req, res, next),
    errors(),
    routeJoiErrorHandler,
  );

  route.get(
    '',
    celebrate({
      query: Joi.object({
        minFloor: Joi.number()
          .integer()
          .min(0)
          .required(),
        maxFloor: Joi.number()
          .integer()
          .min(0)
          .required(),
      }).optional(),
    }),
    (req, res, next) => controller.getBuildingByFloorRange(req, res, next),
    errors(),
    routeJoiErrorHandler,
  );

  protectedRoute.post(
    '/:code/elevator',
    celebrate({
      body: Joi.object({
        number: Joi.number().required(),
        make: Joi.string(),
        model: Joi.string(),
        serialNumber: Joi.string(),
        description: Joi.string(),
        floors: Joi.array()
          .items(Joi.number())
          .required(),
      }),
    }),
    (req, res, next) => controller.createElevator(req, res, next),
    errors(),
    routeJoiErrorHandler,
  );

  protectedRoute.put(
    '/:code/elevator',
    celebrate({
      body: Joi.object({
        make: Joi.string().required(),
        model: Joi.string().required(),
        serialNumber: Joi.string().required(),
        description: Joi.string().required(),
      }),
    }),
    (req, res, next) => controller.updateElevator(req, res, next),
    errors(),
    routeJoiErrorHandler,
  );

  protectedRoute.patch(
    '/:code/elevator',
    celebrate({
      body: Joi.object({
        make: Joi.string(),
        model: Joi.string(),
        serialNumber: Joi.string(),
        description: Joi.string(),
      }),
    }),
    (req, res, next) => controller.updateElevator(req, res, next),
    errors(),
    routeJoiErrorHandler,
  );

  route.get('/all', (req, res, next) => controller.listAllBuildings(req, res, next));

  route.get('/:code/elevators', (req, res, next) => controller.listElevatorsInBuilding(req, res, next));

  route.get('/:code', (req, res, next) => controller.getBuildingByCode(req, res, next));
};
