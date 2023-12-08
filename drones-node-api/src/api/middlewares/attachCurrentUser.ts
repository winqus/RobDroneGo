import { Container } from 'typedi';

import winston from 'winston';

import config from '../../../config';

import { UserMap } from '../../mappers/UserMap';
import IUserRepo from '../../services/IRepos/IUserRepo';

/**
 * Attach user to req.user
 * @param {*} req Express req Object
 * @param {*} res  Express res Object
 * @param {*} next  Express next Function
 */
const attachCurrentUser = async (req, res, next) => {
  const Logger = Container.get('logger') as winston.Logger;
  try {
    const userRepo = Container.get(config.repos.user.name) as IUserRepo;

    if (!req.token || req.token == undefined) {
      return res.status(401).json({ message: 'Non-existent or invalid token' });
    }

    const id = req.token.id;

    const user = await userRepo.findById(id);

    if (user) {
      const userDTO = UserMap.toDTO(user);
      req.user = userDTO;
      next();
    } else {
      return res.status(401).json({ message: 'Token does not correspond to any user in the system' });
    }
  } catch (error) {
    Logger.error('🔥 Error attaching user to req: %o', error);

    return next(error);
  }
};

export default attachCurrentUser;
