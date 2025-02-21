import { errors } from 'celebrate';
import { Router } from 'express';
import { Container } from 'typedi';
import config from '../../../config';
import IFileController from '../../controllers/IControllers/IFileController';
import { UserRole } from '../../domain/userRole.enum';
import middlewares from '../middlewares';
import routeJoiErrorHandler from '../middlewares/routeJoiErrorHandler';
import uploadFilesMiddleware from '../middlewares/uploadFile';

const route = Router();
const protectedRoute = Router();

protectedRoute.use(middlewares.isAuth);
protectedRoute.use(middlewares.attachCurrentUser);
protectedRoute.use(middlewares.requireAnyRole());

export default (app: Router) => {
  app.use('/folder', route);
  app.use('/folder', protectedRoute);

  const controller = Container.get(config.controllers.file.name) as IFileController;

  protectedRoute.post(
    '/upload',
    middlewares.requireAnyRole(config.userRoles.filter((role) => role !== UserRole.User)),
    uploadFilesMiddleware,
    (req, res, next) => controller.uploadFile(req, res, next),
    errors(),
    routeJoiErrorHandler,
  );

  protectedRoute.get(
    '/download/:fileName',
    (req, res, next) => controller.downloadFile(req, res, next),
    errors(),
    routeJoiErrorHandler,
  );

  protectedRoute.get('', (req, res, next) => controller.listAllFiles(req, res, next));
};
