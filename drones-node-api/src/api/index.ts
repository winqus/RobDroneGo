import { Router } from 'express';
import building from './routes/buildingRoute';
import files from './routes/fileRoute';
import floor from './routes/floorRoute';
import passage from './routes/passageRoute';
import planning from './routes/planningRoute';
import robot from './routes/robotRoute';
import robotType from './routes/robotTypeRoute';
import role from './routes/roleRoute';
import room from './routes/roomRoute';
import { default as auth, default as user } from './routes/userRoute';

export default () => {
  const app = Router();

  auth(app);
  user(app);
  role(app);
  building(app);
  floor(app);
  room(app);
  passage(app);
  robotType(app);
  robot(app);
  files(app);
  planning(app);

  return app;
};
