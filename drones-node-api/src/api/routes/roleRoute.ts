import { celebrate, Joi } from 'celebrate';
import { Router } from 'express';

import { Container } from 'typedi';
import IRoleController from '../../controllers/IControllers/IRoleController';

import config from '../../../config';
import { UserRole } from '../../domain/userRole.enum';
import middlewares from '../middlewares';

const route = Router();
const protectedRoute = Router();

protectedRoute.use(middlewares.isAuth);
protectedRoute.use(middlewares.attachCurrentUser);
protectedRoute.use(middlewares.requireAnyRole([UserRole.SystemAdministrator]));

export default (app: Router) => {
  app.use('/roles', route);
  app.use('/roles', protectedRoute);
  const ctrl = Container.get(config.controllers.role.name) as IRoleController;

  protectedRoute.post(
    '',
    celebrate({
      body: Joi.object({
        name: Joi.string().required(),
      }),
    }),
    (req, res, next) => ctrl.createRole(req, res, next),
  );

  protectedRoute.put(
    '',
    celebrate({
      body: Joi.object({
        id: Joi.string().required(),
        name: Joi.string().required(),
      }),
    }),
    (req, res, next) => ctrl.updateRole(req, res, next),
  );
};
