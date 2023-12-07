import config from '../../../config';

/**
 * Require any of the given roles.
 * @param {*} roles Array of roles to require.
 * If no roles are given, defaults to check for any user role.
 */
const requireAnyRole = (roles = config.userRoles) => {
  return async (req, res, next) => {
    try {
      if (!req.user) {
        res.status(401).send({ message: 'Unauthorized: No user provided' });
      }

      if (req.user && roles.includes(req.user.role)) {
        next();
      } else {
        res.status(403).send({ message: 'Forbidden: Insufficient permissions' });
      }
    } catch (error) {
      next(error);
    }
  };
};

export default requireAnyRole;
