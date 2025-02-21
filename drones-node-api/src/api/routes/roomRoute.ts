import { celebrate, errors, Joi } from 'celebrate';
import { Router } from 'express';
import { Container } from 'typedi';
import config from '../../../config';
import IRoomController from '../../controllers/IControllers/IRoomController';
import { UserRole } from '../../domain/userRole.enum';
import middlewares from '../middlewares';
import routeJoiErrorHandler from '../middlewares/routeJoiErrorHandler';

const route = Router();
const protectedRoute = Router();

protectedRoute.use(middlewares.isAuth);
protectedRoute.use(middlewares.attachCurrentUser);
protectedRoute.use(middlewares.requireAnyRole([UserRole.CampusManager]));

export default (app: Router) => {
  app.use('/room', route);
  app.use('/room', protectedRoute);

  const controller = Container.get(config.controllers.room.name) as IRoomController;

  protectedRoute.post(
    '',
    celebrate({
      body: Joi.object({
        name: Joi.string().required(),
        description: Joi.string(),
        size: Joi.object({
          width: Joi.number()
            .integer()
            .required(),
          length: Joi.number()
            .integer()
            .required(),
        }).required(),
        position: Joi.object({
          x: Joi.number().required(),
          y: Joi.number().required(),
        }).required(),
        category: Joi.string().required(),
        floorId: Joi.string().required(),
      }),
    }),
    (req, res, next) => controller.createRoom(req, res, next),
    errors(),
    routeJoiErrorHandler,
  );

  route.get('/all', (req, res, next) => controller.getAllRooms(req, res, next), routeJoiErrorHandler);
};
