import { celebrate, errors, Joi } from 'celebrate';
import { Router } from 'express';
import { Container } from 'typedi';
import config from '../../../config';
import IRobotController from '../../controllers/IControllers/IRobotController';
import routeJoiErrorHandler from '../middlewares/routeJoiErrorHandler';

const route = Router();

export default (app: Router) => {
  app.use('/robot', route);

  const controller = Container.get(config.controllers.robot.name) as IRobotController;

  route.post(
    '',
    celebrate({
      body: Joi.object({
        code: Joi.string().required(),
        nickname: Joi.string().required(),
        serialNumber: Joi.string().required(),
        description: Joi.string()
          .allow('')
          .optional(),
        type: Joi.string().required(),
      }),
    }),
    (req, res, next) => controller.createRobot(req, res, next),
    errors(),
    routeJoiErrorHandler,
  );
};
