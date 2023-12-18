import { Router } from 'express';
import { Container } from 'typedi';

import { celebrate, errors, Joi } from 'celebrate';
import config from '../../../config';
import IUserController from '../../controllers/IControllers/IUserController';
import { UserRole } from '../../domain/userRole.enum';
import middlewares from '../middlewares';
import routeJoiErrorHandler from '../middlewares/routeJoiErrorHandler';

const route = Router();
const protectedRoute = Router();

protectedRoute.use(middlewares.isAuth);
protectedRoute.use(middlewares.attachCurrentUser);

export default (app: Router) => {
  app.use('/auth', route);
  app.use('/auth', protectedRoute);

  const controller = Container.get(config.controllers.user.name) as IUserController;

  route.post(
    '/signup',
    celebrate({
      body: Joi.object({
        firstName: Joi.string().required(),
        lastName: Joi.string().required(),
        email: Joi.string().required(),
        phonenumber: Joi.string().required(),
        taxpayernumber: Joi.string(),
        password: Joi.string().required(),
        role: Joi.string(),
      }),
    }),
    async (req, res, next) => controller.signUp(req, res, next),
    errors(),
    routeJoiErrorHandler,
  );

  route.post(
    '/signin',
    celebrate({
      body: Joi.object({
        email: Joi.string().required(),
        password: Joi.string().required(),
      }),
    }),
    async (req, res, next) => controller.signIn(req, res, next),
    errors(),
    routeJoiErrorHandler,
  );

  protectedRoute.patch(
    '/update',
    celebrate({
      body: Joi.object({
        firstName: Joi.string(),
        lastName: Joi.string(),
        email: Joi.string(),
        phonenumber: Joi.string(),
        taxpayernumber: Joi.string(),
        password: Joi.string(),
      }),
    }),
    async (req, res, next) => controller.updateUser(req, res, next),
    errors(),
    routeJoiErrorHandler,
  );

  protectedRoute.delete(
    '/delete',
    async (req, res, next) => controller.deleteUser(req, res, next),
    errors(),
    routeJoiErrorHandler,
  );

  protectedRoute.patch(
    '/confirm',
    celebrate({
      body: Joi.object({
        email: Joi.string().required(),
        isConfirmed: Joi.boolean().default(true),
      }),
    }),
    async (req, res, next) => controller.confirmUser(req, res, next),
    errors(),
    routeJoiErrorHandler,
  );

  route.post('/logout', middlewares.isAuth, (req, res, next) => controller.signOut(req, res, next));

  protectedRoute.get('/me', middlewares.attachCurrentUser, async (req, res, next) => controller.getMe(req, res, next));

  protectedRoute.get('/all', middlewares.requireAnyRole([UserRole.SystemAdministrator]), async (req, res, next) =>
    controller.getAllUsers(req, res, next),
  );
};
