import { NextFunction, Request, Response } from 'express';

import { Container, Inject, Service } from 'typedi';

import config from '../../config';

import winston from 'winston';
import { IUserDTO } from '../dto/IUserDTO';
import IUserService from '../services/IServices/IUserService';
import IUserController from './IControllers/IUserController';

@Service()
export default class UserController implements IUserController {
  private logger: winston.Logger;

  constructor(@Inject(config.services.user.name) private userService: IUserService) {
    this.logger = Container.get('logger');
  }

  public async signUp(req: Request, res: Response, next: NextFunction) {
    try {
      const userOrError = await this.userService.signUp({
        ...req.body,
        isConfirmed: false,
      } as IUserDTO);

      if (userOrError.isFailure) {
        this.logger.debug(userOrError.errorValue());

        return res.status(401).send(userOrError.errorValue());
      }

      const { userDTO } = userOrError.getValue();

      return res.status(201).json({ userDTO });
    } catch (error) {
      this.logger.error('User signup error %o', error);

      return next(error);
    }
  }

  public async signIn(req: Request, res: Response, next: NextFunction) {
    try {
      const { email, password } = req.body;
      const result = await this.userService.signIn(email, password);

      if (result.isFailure) {
        return res.status(403).json({ message: result.errorValue() });
      }

      const { userDTO, token } = result.getValue();

      return res.json({ userDTO, token }).status(200);
    } catch (error) {
      this.logger.error('User sign-in error %o', error);

      return next(error);
    }
  }

  public async signOut(req: Request, res: Response, _next: NextFunction) {
    this.logger.debug('Sign-out user, body %o', req.body);

    return res.status(200).send({ message: 'Logged out successfully' });
  }

  public async getMe(req: any, res: Response, next: NextFunction) {
    try {
      if (!req.token || req.token == undefined) {
        return res.json(new Error('Token invalid')).status(401);
      }

      const userResult = await this.userService.getUserById(req.token.id);
      if (userResult.isFailure) {
        return res.json(new Error('No such user')).status(401);
      }

      const userDTO = userResult.getValue();

      return res.json(userDTO).status(200);
    } catch (error) {
      this.logger.error('Failed to getMe user %o', error);

      return next(error);
    }
  }

  public async deleteUser(req: Request & any, res: Response, next: NextFunction) {
    try {
      if (!req.token || req.token == undefined) {
        return res.json(new Error('Token invalid')).status(401);
      }

      const userResult = await this.userService.getUserById(req.token.id);
      if (userResult.isFailure) {
        return res.json(new Error('No such user')).status(401);
      }

      this.userService.deleteUser(req.token.email);

      this.logger.debug('User deleted, user email %o', req.token.email);

      return res.json({ message: 'User deleted' }).status(200);
    } catch (error) {
      this.logger.error('Failed to delete user %o', error);

      return next(error);
    }
  }

  public async updateUser(req: Request & any, res: Response, next: NextFunction) {
    try {
      const { email } = req.token;
      const userDTO = req.body as Partial<IUserDTO>;

      const result = await this.userService.updateUser(email, userDTO);

      if (result.isFailure) {
        return res.status(400).json({ message: result.errorValue() });
      }

      return res.status(200).send(result.getValue());
    } catch (error) {
      this.logger.error('Error updating user: %o', error);

      return next(error);
    }
  }

  public async confirmUser(req: Request, res: Response, next: NextFunction) {
    try {
      const { email, isConfirmed } = req.body;
      const result = await this.userService.setUserConfirmation(email, isConfirmed);

      if (result.isFailure) {
        return res.status(400).json({ message: result.errorValue() });
      }

      return res.status(200).send({ message: 'User confirmation changed successfully' });
    } catch (error) {
      this.logger.error('Error confirming user: %o', error);

      return next(error);
    }
  }

  public async getAllUsers(req: Request, res: Response, next: NextFunction) {
    try {
      const users = await this.userService.getAllUsers();

      if (users.isFailure) {
        return res.status(400).json({ message: users.errorValue() });
      }

      return res.status(200).json(users.getValue());
    } catch (error) {
      this.logger.error('Error retrieving all users: %o', error);

      return next(error);
    }
  }
}
