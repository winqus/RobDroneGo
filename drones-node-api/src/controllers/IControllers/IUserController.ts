import { NextFunction, Request, Response } from 'express';

export default interface IUserController {
  signUp(req: Request, res: Response, next: NextFunction);
  signIn(req: Request, res: Response, next: NextFunction);
  signOut(req: Request, res: Response, next: NextFunction);
  getMe(req: Request, res: Response, next: NextFunction);
  deleteUser(req: Request, res: Response, next: NextFunction);
  updateUser(req: Request, res: Response, next: NextFunction);
  confirmUser(req: Request, res: Response, next: NextFunction);
  getAllUsers(req: Request, res: Response, next: NextFunction);
}
