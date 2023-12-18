import { celebrate, errors, Joi } from 'celebrate';
import { Router } from 'express';
import { Container } from 'typedi';
import config from '../../../config';

import IFloorController from '../../controllers/IControllers/IFloorController';
import { UserRole } from '../../domain/userRole.enum';
import middlewares from '../middlewares';
import routeJoiErrorHandler from '../middlewares/routeJoiErrorHandler';

const route = Router();
const protectedRoute = Router();

protectedRoute.use(middlewares.isAuth);
protectedRoute.use(middlewares.attachCurrentUser);
protectedRoute.use(middlewares.requireAnyRole([UserRole.CampusManager]));

export default (app: Router) => {
  app.use('/floor', route);
  app.use('/floor', protectedRoute);

  const controller = Container.get(config.controllers.floor.name) as IFloorController;

  protectedRoute.post(
    '',
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

  protectedRoute.put(
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

  protectedRoute.patch(
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

  protectedRoute.patch(
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
