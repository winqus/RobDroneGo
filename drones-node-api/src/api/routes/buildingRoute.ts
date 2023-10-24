import { celebrate, Joi } from 'celebrate';
import { Router } from 'express';
import { Container } from 'typedi';
import config from '../../../config';
import IBuildingController from '../../controllers/IControllers/IBuildingController';

const route = Router();

export default (app: Router) => {
  app.use('/building', route);

  const controller = Container.get(config.controllers.building.name) as IBuildingController;

  route.post(
    '',
    celebrate({
      body: Joi.object({
        name: Joi.string()
          .allow('')
          .optional(),
        code: Joi.string().required(),
        description: Joi.string()
          .allow('')
          .optional(),
        floorSizeLength: Joi.number().required(),
        floorSizeWidth: Joi.number().required(),
      }),
    }),
    (req, res, next) => controller.createBuilding(req, res, next),
  );
};
