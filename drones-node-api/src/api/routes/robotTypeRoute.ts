import { celebrate, errors, Joi } from 'celebrate';
import { Router } from 'express';
import { Container } from 'typedi';
import config from '../../../config';
import IRobotTypeController from '../../controllers/IControllers/IRobotTypeController';
import { UserRole } from '../../domain/userRole.enum';
import middlewares from '../middlewares';
import routeJoiErrorHandler from '../middlewares/routeJoiErrorHandler';

const route = Router();
const protectedRoute = Router();

protectedRoute.use(middlewares.isAuth);
protectedRoute.use(middlewares.attachCurrentUser);
protectedRoute.use(middlewares.requireAnyRole([UserRole.FleetManager]));

export default (app: Router) => {
  app.use('/robotType', route);
  app.use('/robotType', protectedRoute);

  const controller = Container.get(config.controllers.robotType.name) as IRobotTypeController;

  protectedRoute.post(
    '',
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
