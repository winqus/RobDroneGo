import { celebrate, errors, Joi } from 'celebrate';
import { Router } from 'express';
import { Container } from 'typedi';
import config from '../../../config';
import IRobotTypeController from '../../controllers/IControllers/IRobotTypeController';
import middlewares from '../middlewares';
import routeJoiErrorHandler from '../middlewares/routeJoiErrorHandler';

const route = Router();

export default (app: Router) => {
  app.use('/robotType', route);

  const controller = Container.get(config.controllers.robotType.name) as IRobotTypeController;

  route.post(
    '',
    middlewares.isAuth,
    celebrate({
      body: Joi.object({
        name: Joi.string().required(),
        brand: Joi.string().required(),
        model: Joi.string().required(),
        typesOfTasks: Joi.array().items(Joi.string()),
      }),
    }),
    (req, res, next) => controller.createRobotType(req, res, next),
    errors(),
    routeJoiErrorHandler,
  );
};
