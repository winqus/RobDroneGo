import { celebrate, errors, Joi } from 'celebrate';
import { Request, Response, Router } from 'express';
import { Container } from 'typedi';
import config from '../../../config';
import IBuildingController from '../../controllers/IControllers/IBuildingController';
import BuildingRepo from '../../repos/buildingRepo';
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
  );
};
