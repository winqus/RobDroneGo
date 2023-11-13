import { celebrate, errors, Joi } from 'celebrate';
import { Router } from 'express';
import { Container } from 'typedi';
import config from '../../../config';
import IBuildingController from '../../controllers/IControllers/IBuildingController';
import routeJoiErrorHandler from '../middlewares/routeJoiErrorHandler';

const route = Router();

export default (app: Router) => {
  app.use('/building', route);

  const controller = Container.get(config.controllers.building.name) as IBuildingController;

  route.post(
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

  route.put(
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

  route.patch(
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

  route.post(
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

  route.put(
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

  route.patch(
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
