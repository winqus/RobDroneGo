import { NextFunction, Request, Response } from 'express';

export default interface IFileController {
  uploadFile(req: Request, res: Response, next: NextFunction);
  downloadFile(req: Request, res: Response, next: NextFunction);
  listAllFiles(req: Request, res: Response, next: NextFunction);
}
