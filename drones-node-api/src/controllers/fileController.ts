import { NextFunction, Request, Response } from 'express';
import { Inject, Service } from 'typedi';
import config from '../../config';
import { Result } from '../core/logic/Result';
import IFileService from '../services/IServices/IFileService';
import IFileController from './IControllers/IFileController';

@Service()
export default class FileController implements IFileController {
  constructor(@Inject(config.services.file.name) private fileService: IFileService) {}

  public async uploadFile(req: Request, res: Response, next: NextFunction) {
    try {
      const fileExists = (await this.fileService.uploadFile(res.locals.file)) as Result<boolean>;

      if (fileExists.isFailure) {
        return res.status(400).json({ message: fileExists.error.toString() });
      }

      const file = fileExists.getValue();

      return res.status(201).json(file);
    } catch (error) {
      return next(error);
    }
  }

  public async downloadFile(req: Request, res: Response, next: NextFunction) {
    try {
      const fileName = req.params.fileName as string;
      const fileOrError = (await this.fileService.downloadFile(fileName)) as Result<string>;

      if (fileOrError.isFailure) {
        return res.status(400).json({ message: fileOrError.error.toString() });
      }

      const file = fileOrError.getValue();

      return res.download(file);
    } catch (error) {
      return next(error);
    }
  }

  public async listAllFiles(req: Request, res: Response, next: NextFunction) {
    try {
      const filesResult = await this.fileService.listAllFiles();

      if (filesResult.isFailure) {
        return res.status(400).json({ message: filesResult.error.toString() });
      }

      const files = filesResult.getValue();

      return res.status(200).json(files);
    } catch (error) {
      return next(error);
    }
  }
}
