import { celebrate, errors, Joi } from 'celebrate';
import { Router } from 'express';
import { Container } from 'typedi';
import config from '../../../config';
import IRobotController from '../../controllers/IControllers/IRobotController';
import { UserRole } from '../../domain/userRole.enum';
import middlewares from '../middlewares';
import routeJoiErrorHandler from '../middlewares/routeJoiErrorHandler';

const route = Router();
const protectedRoute = Router();

protectedRoute.use(middlewares.isAuth);
protectedRoute.use(middlewares.attachCurrentUser);
protectedRoute.use(middlewares.requireAnyRole([UserRole.FleetManager]));

export default (app: Router) => {
  app.use('/robot', route);
  app.use('/robot', protectedRoute);

  const controller = Container.get(config.controllers.robot.name) as IRobotController;

  protectedRoute.post(
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

  protectedRoute.patch(
    '/:robotCode/state',
    celebrate({
      body: Joi.object({
        available: Joi.boolean().required(),
      }),
      params: Joi.object({
        robotCode: Joi.string().required(),
      }),
    }),
    (req, res, next) => controller.changeRobotState(req, res, next),
    errors(),
    routeJoiErrorHandler,
  );

  route.get(
    '/',
    celebrate({
      query: Joi.object({
        id: Joi.string().optional(),
        name: Joi.string().optional(),
        type: Joi.string().optional(),
        brand: Joi.string().optional(),
        model: Joi.string().optional(),
        typesOfTasks: Joi.array()
          .items(Joi.string())
          .single()
          .optional(),
      }),
    }),
    async (req, res, next) => {
      if (Object.keys(req.query).length > 0) {
        return controller.getByType(req, res, next);
      }

      return controller.listAllRobots(req, res, next);
    },
    errors(),
    routeJoiErrorHandler,
  );
};
