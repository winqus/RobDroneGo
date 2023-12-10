import { celebrate, errors, Joi } from 'celebrate';
import { Router } from 'express';
import { Container } from 'typedi';
import config from '../../../config';
import ITaskRequestController from '../../controllers/IControllers/ITaskRequestController';
import { TaskStatus } from '../../domain/TaskRequest/taskStatus';
import { UserRole } from '../../domain/userRole.enum';
import middlewares from '../middlewares';
import routeJoiErrorHandler from '../middlewares/routeJoiErrorHandler';

const route = Router();
const protectedRoute = Router();

protectedRoute.use(middlewares.isAuth);
protectedRoute.use(middlewares.attachCurrentUser);
protectedRoute.use(middlewares.requireAnyRole([UserRole.TaskManager]));

export default (app: Router) => {
  app.use('/taskRequest', route);
  app.use('/taskRequest', protectedRoute);

  const controller = Container.get(config.controllers.taskRequest.name) as ITaskRequestController;

  route.post(
    '',
    celebrate({
      body: Joi.object({
        requesterEmail: Joi.string().required(),
        task: Joi.alternatives()
          .try(
            Joi.object({
              pickUpRoomId: Joi.string().required(),
              deliveryRoomId: Joi.string().required(),
              pickUpContact: Joi.number().required(),
              pickUpName: Joi.string().required(),
              deliveryContact: Joi.number().required(),
              deliveryName: Joi.string().required(),
              description: Joi.string().required(),
              confirmationCode: Joi.number().required(),
            }),
            Joi.object({
              buildingCode: Joi.string().required(),
              floorNumber: Joi.array()
                .items(Joi.number())
                .required(),
              contactNumber: Joi.number().required(),
            }),
          )
          .required(),
      }),
    }),
    (req, res, next) => controller.create(req, res, next),
    errors(),
    routeJoiErrorHandler,
  );

  route.patch(
    '/:id/status',
    celebrate({
      body: Joi.object({
        status: Joi.string()
          .valid(Object.values(TaskStatus))
          .required(),
      }),
      params: Joi.object({
        id: Joi.string().required(),
      }),
    }),
    (req, res, next) => controller.changeState(req, res, next),
    errors(),
    routeJoiErrorHandler,
  );

  route.patch(
    '/:id/navigationData',
    celebrate({
      body: Joi.object({
        floorsPaths: Joi.array().items(
          Joi.object({
            fromBuilding: Joi.string().required(),
            fromFloorNumber: Joi.string().required(),
            toBuilding: Joi.string().required(),
            toFloorNumber: Joi.string().required(),
            type: Joi.string().required(),
          }),
        ),
        mapPathCount: Joi.number().required(),
        mapPaths: Joi.array().items(
          Joi.object({
            buildingCode: Joi.string().required(),
            cost: Joi.number().required(),
            floorNumber: Joi.number().required(),
            path: Joi.array().items(
              Joi.object({
                col: Joi.number().required(),
                row: Joi.number().required(),
              }),
            ),
          }),
        ),
      }),
      params: Joi.object({
        id: Joi.string().required(),
      }),
    }),
    (req, res, next) => controller.addNavigationData(req, res, next),
    errors(),
    routeJoiErrorHandler,
  );

  route.get('', (req, res, next) => controller.getAll(req, res, next));
  route.get('/:id', (req, res, next) => controller.getById(req, res, next));
};
