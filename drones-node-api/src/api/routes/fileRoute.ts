import { errors } from 'celebrate';
import { Router } from 'express';
import { Container } from 'typedi';
import config from '../../../config';
import IFileController from '../../controllers/IControllers/IFileController';
import routeJoiErrorHandler from '../middlewares/routeJoiErrorHandler';
import uploadFilesMiddleware from '../middlewares/uploadFile';

const route = Router();

export default (app: Router) => {
  app.use('/folder', route);

  const controller = Container.get(config.controllers.file.name) as IFileController;

  route.post(
    '/upload',
    uploadFilesMiddleware,
    (req, res, next) => controller.uploadFile(req, res, next),
    errors(),
    routeJoiErrorHandler,
  );

  route.get(
    '/download/:fileName',
    (req, res, next) => controller.downloadFile(req, res, next),
    errors(),
    routeJoiErrorHandler,
  );

  route.get('', (req, res, next) => controller.listAllFiles(req, res, next));
};
