import { NextFunction, Request, Response } from 'express';
import { Inject, Service } from 'typedi';
import config from '../../config';

import IRoleDTO from '../dto/IRoleDTO';
import IRoleService from '../services/IServices/IRoleService';
import IRoleController from './IControllers/IRoleController';

import { Result } from '../core/logic/Result';

@Service()
export default class RoleController implements IRoleController /* TODO: extends ../core/infra/BaseController */ {
  constructor(@Inject(config.services.role.name) private roleServiceInstance: IRoleService) {}

  public async createRole(req: Request, res: Response, next: NextFunction) {
    try {
      const roleOrError = (await this.roleServiceInstance.createRole(req.body as IRoleDTO)) as Result<IRoleDTO>;

      if (roleOrError.isFailure) {
        return res.status(400).json({ message: roleOrError.errorValue() });
      }

      const roleDTO = roleOrError.getValue();

      return res.status(201).json(roleDTO);
    } catch (e) {
      return next(e);
    }
  }

  public async updateRole(req: Request, res: Response, next: NextFunction) {
    try {
      const roleOrError = (await this.roleServiceInstance.updateRole(req.body as IRoleDTO)) as Result<IRoleDTO>;

      if (roleOrError.isFailure) {
        return res.status(404).send();
      }

      const roleDTO = roleOrError.getValue();

      return res.status(201).json(roleDTO);
    } catch (e) {
      return next(e);
    }
  }
}
