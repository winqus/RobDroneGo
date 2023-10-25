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

  route.get('', (req, res, next) => controller.listAllBuildings(req, res, next));

  route.get('/:code', (req, res, next) => controller.getBuildingByCode(req, res, next));
};
