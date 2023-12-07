import { errors } from 'celebrate';
import { Router } from 'express';
import { Container } from 'typedi';
import config from '../../../config';
import IFileController from '../../controllers/IControllers/IFileController';
import middlewares from '../middlewares';
import routeJoiErrorHandler from '../middlewares/routeJoiErrorHandler';
import uploadFilesMiddleware from '../middlewares/uploadFile';

const route = Router();

export default (app: Router) => {
  app.use('/folder', route);

  const controller = Container.get(config.controllers.file.name) as IFileController;

  route.post(
    '/upload',
    middlewares.isAuth,
    uploadFilesMiddleware,
    (req, res, next) => controller.uploadFile(req, res, next),
    errors(),
    routeJoiErrorHandler,
  );

  route.get(
    '/download/:fileName',
    middlewares.isAuth,
    (req, res, next) => controller.downloadFile(req, res, next),
    errors(),
    routeJoiErrorHandler,
  );

  route.get('', middlewares.isAuth, (req, res, next) => controller.listAllFiles(req, res, next));
};
