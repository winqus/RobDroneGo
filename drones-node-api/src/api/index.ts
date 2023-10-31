import { Router } from 'express';
import building from './routes/buildingRoute';
import floor from './routes/floorRoute';
import passage from './routes/passageRoute';
import robotType from './routes/robotTypeRoute';
import role from './routes/roleRoute';
import { default as auth, default as user } from './routes/userRoute';

export default () => {
  const app = Router();

  auth(app);
  user(app);
  role(app);
  building(app);
  floor(app);
  passage(app);
  robotType(app);

  return app;
};
