import { Router } from 'express';
import { Container } from 'typedi';

import { celebrate, errors, Joi } from 'celebrate';
import config from '../../../config';
import IUserController from '../../controllers/IControllers/IUserController';
import middlewares from '../middlewares';
import routeJoiErrorHandler from '../middlewares/routeJoiErrorHandler';

const route = Router();

export default (app: Router) => {
  app.use('/auth', route);

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

  route.patch(
    '/update',
    middlewares.isAuth,
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

  route.delete(
    '/delete',
    middlewares.isAuth,
    async (req, res, next) => controller.deleteUser(req, res, next),
    errors(),
    routeJoiErrorHandler,
  );

  route.patch(
    '/confirm',
    middlewares.isAuth,
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

  route.get('/me', middlewares.isAuth, middlewares.attachCurrentUser, async (req, res, next) =>
    controller.getMe(req, res, next),
  );

  route.get('/all', middlewares.isAuth, async (req, res, next) => controller.getAllUsers(req, res, next));
};
