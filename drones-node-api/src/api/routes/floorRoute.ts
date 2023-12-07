import { celebrate, errors, Joi } from 'celebrate';
import { Router } from 'express';
import { Container } from 'typedi';
import config from '../../../config';

import IFloorController from '../../controllers/IControllers/IFloorController';
import middlewares from '../middlewares';
import routeJoiErrorHandler from '../middlewares/routeJoiErrorHandler';

const route = Router();

export default (app: Router) => {
  app.use('/floor', route);

  const controller = Container.get(config.controllers.floor.name) as IFloorController;

  route.post(
    '',
    middlewares.isAuth,
    celebrate({
      body: Joi.object({
        floorNumber: Joi.number().required(),
        description: Joi.string()
          .allow('')
          .optional(),
        buildingCode: Joi.string().required(),
      }),
    }),
    (req, res, next) => controller.createFloor(req, res, next),
    errors(),
    routeJoiErrorHandler,
  );

  route.put(
    '/:floorId',
    middlewares.isAuth,
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
    middlewares.isAuth,
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
    middlewares.isAuth,
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
          exitLocations: Joi.object({
            passages: Joi.array()
              .items(
                Joi.object({
                  cellPosition: Joi.array()
                    .items(Joi.number())
                    .required(),
                  destination: Joi.object({
                    buildingCode: Joi.string().required(),
                    floorNumber: Joi.number().required(),
                  }).required(),
                }),
              )
              .required(),
            elevators: Joi.array()
              .items(
                Joi.object({
                  cellPosition: Joi.array()
                    .items(Joi.number())
                    .required(),
                }),
              )
              .required(),
          }).required(),
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

  route.get('/:floorNumber/building/:buildingCode/map', (req, res, next) => controller.getMap(req, res, next));
};
