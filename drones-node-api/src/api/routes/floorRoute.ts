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

  route.get('', (req, res, next) => controller.listAllFloors(req, res, next));
  route.get('/:buildingCode', (req, res, next) => controller.getFloorsByBuildingCode(req, res, next));
};
