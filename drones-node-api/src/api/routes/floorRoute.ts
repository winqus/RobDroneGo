import { celebrate, errors, Joi } from 'celebrate';
import { Router } from 'express';
import { Container } from 'typedi';
import config from '../../../config';

import IFloorController from '../../controllers/IControllers/IFloorController';
import routeJoiErrorHandler from '../middlewares/routeJoiErrorHandler';

const route = Router();

export default (app: Router) => {
  app.use('/floor', route);

  const controller = Container.get(config.controllers.floor.name) as IFloorController;

  route.post(
    '',
    celebrate({
      body: Joi.object({
        floorNumber: Joi.number().required(),
        description: Joi.string()
          .allow('')
          .optional(),
        servedByElevator: Joi.boolean().required(),
        buildingCode: Joi.string().required(),
      }),
    }),
    (req, res, next) => controller.createFloor(req, res, next),
    errors(),
    routeJoiErrorHandler,
  );

  route.put(
    '/:floorId',
    celebrate({
      body: Joi.object({
        description: Joi.string()
          .allow('')
          .optional(),
      }),
      params: Joi.object({
        floorId: Joi.string().required(),
      }),
    }),
    (req, res, next) => controller.partialUpdateFloor(req, res, next),
    errors(),
    routeJoiErrorHandler,
  );

  route.patch(
    '/:floorId',
    celebrate({
      body: Joi.object({
        description: Joi.string()
          .allow('')
          .optional(),
      }),
      params: Joi.object({
        floorId: Joi.string().required(),
      }),
    }),
    (req, res, next) => controller.partialUpdateFloor(req, res, next),
    errors(),
    routeJoiErrorHandler,
  );

  route.get(
    '/elevator',
    celebrate({
      query: Joi.object({
        buildingCode: Joi.string().required(),
      }),
    }),
    (req, res, next) => controller.getFloorsServedByElevator(req, res, next),
    errors(),
    routeJoiErrorHandler,
  );

  route.get('', (req, res, next) => controller.listAllFloors(req, res, next));

  route.get('/:buildingCode', (req, res, next) => controller.getFloorsByBuildingCode(req, res, next));

  route.patch(
    '/:floorNumber/building/:buildingCode',
    celebrate({
      body: Joi.object({
        map: Joi.object({
          size: Joi.object({
            width: Joi.number().required(),
            height: Joi.number().required(),
          }).required(),
          map: Joi.array()
            .items(Joi.array().items(Joi.number()))
            .required(),
        }).required(),
      }),
      params: Joi.object({
        floorNumber: Joi.number().required(),
        buildingCode: Joi.string().required(),
      }),
    }),
    (req, res, next) => controller.loadMap(req, res, next),
    errors(),
    routeJoiErrorHandler,
  );
};
