import { celebrate, errors, Joi } from 'celebrate';
import { Router } from 'express';
import { Container } from 'typedi';
import config from '../../../config';
import IPassageController from '../../controllers/IControllers/IPassageController';
import routeJoiErrorHandler from '../middlewares/routeJoiErrorHandler';

const route = Router();

export default (app: Router) => {
  app.use('/passage', route);

  const controller = Container.get(config.controllers.passage.name) as IPassageController;

  route.post(
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
};
