import { celebrate, Joi } from 'celebrate';
import { Router } from 'express';

import { Container } from 'typedi';
import IRoleController from '../../controllers/IControllers/IRoleController';

import config from '../../../config';
import middlewares from '../middlewares';

const route = Router();

export default (app: Router) => {
  app.use('/roles', route);

  const ctrl = Container.get(config.controllers.role.name) as IRoleController;

  route.post(
    '',
    middlewares.isAuth,
    celebrate({
      body: Joi.object({
        name: Joi.string().required(),
      }),
    }),
    (req, res, next) => ctrl.createRole(req, res, next),
  );

  route.put(
    '',
    middlewares.isAuth,
    celebrate({
      body: Joi.object({
        id: Joi.string().required(),
        name: Joi.string().required(),
      }),
    }),
    (req, res, next) => ctrl.updateRole(req, res, next),
  );
};
