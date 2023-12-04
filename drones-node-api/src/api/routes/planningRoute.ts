import { celebrate, errors, Joi } from 'celebrate';
import { Router } from 'express';
import http from 'http';
import querystring from 'querystring';
import { Container } from 'typedi';
import config from '../../../config';
import IPlanningController from '../../controllers/IControllers/IPlanningController';
import routeJoiErrorHandler from '../middlewares/routeJoiErrorHandler';

const routeSchema = {
  query: Joi.object().keys({
    origin_building_code: Joi.string().required(),
    origin_floor_number: Joi.number().required(),
    origin_map_cell_x: Joi.number().required(),
    origin_map_cell_y: Joi.number().required(),
    destination_building_code: Joi.string().required(),
    destination_floor_number: Joi.number().required(),
    destination_map_cell_x: Joi.number().required(),
    destination_map_cell_y: Joi.number().required(),
    minimize_elevator_uses: Joi.boolean().required(),
    minimize_building_count: Joi.boolean().required(),
  }),
};

const routeSchema2 = {
  query: Joi.object().keys({
    origin_building_code: Joi.string().required(),
    origin_floor_number: Joi.number().required(),
    origin_room: Joi.string().required(),
    destination_building_code: Joi.string().required(),
    destination_floor_number: Joi.number().required(),
    destination_room: Joi.string().required(),
    minimize_elevator_uses: Joi.boolean().required(),
    minimize_building_count: Joi.boolean().required(),
  }),
};

const route = Router();
export default (app: Router) => {
  app.use('/planning', route);

  const planningController = Container.get(config.controllers.planning.name) as IPlanningController;

  route.get('/test', (_req, res, next) => {
    const path = `${config.planningAPI.basePath}/test`;

    const options = {
      hostname: config.planningAPI.hostname,
      port: config.planningAPI.port,
      path: path,
      method: 'GET',
    };

    const request = http.request(options, (response) => {
      if (response.headers['content-type']) {
        res.setHeader('Content-Type', response.headers['content-type']);
      }

      let data = '';

      response.on('data', (chunk) => {
        data += chunk;
      });

      response.on('end', () => {
        res.send(data);
      });
    });

    request.on('error', (error) => {
      console.error(`Problem with request: ${error.message}`);

      return next(error);
    });

    request.end();
  });

  route.get(
    '/route',
    celebrate(routeSchema),
    (req, res, next) => {
      const queryString = querystring.stringify(req.query as any);

      const path = `${config.planningAPI.basePath}/route?${queryString}`;

      const options = {
        hostname: config.planningAPI.hostname,
        port: config.planningAPI.port,
        path: path,
        method: 'GET',
      };

      const request = http.request(options, (response) => {
        if (response.headers['content-type']) {
          res.setHeader('Content-Type', response.headers['content-type']);
        }

        let data = '';

        response.on('data', (chunk) => {
          data += chunk;
        });

        response.on('end', () => {
          res.send(data);
        });
      });

      request.on('error', (error) => {
        console.error(`Problem with request: ${error.message}`);

        return next(error);
      });

      request.end();
    },
    errors(),
    routeJoiErrorHandler,
  );

  route.get(
    '/calculate-cells',
    celebrate(routeSchema2),
    async (req, res, next) => {
      try {
        const result = await planningController.calculateCells(req, res, next);

        if (result?.statusCode?.toString() === '400') {
          res.status(400).json({ message: result?.body?.message });

          return next(result?.body?.messag);
        }

        const { origin_map_cell_x, origin_map_cell_y, destination_map_cell_x, destination_map_cell_y } = result;

        const newData = {
          origin_map_cell_x,
          origin_map_cell_y,
          destination_map_cell_x,
          destination_map_cell_y,
        };

        const queryString = querystring.stringify({
          ...req.query,
          ...newData,
        });

        const path = `${config.planningAPI.basePath}/route?${queryString}`;

        const options = {
          hostname: config.planningAPI.hostname,
          port: config.planningAPI.port,
          path: path,
          method: 'GET',
        };

        const request = http.request(options, (response) => {
          if (response.headers['content-type']) {
            res.setHeader('Content-Type', response.headers['content-type']);
          }

          let data = '';

          response.on('data', (chunk) => {
            data += chunk;
          });

          response.on('end', () => {
            res.send(data);
          });
        });

        request.on('error', (error) => {
          console.error(`Problem with request: ${error.message}`);

          return next(error);
        });

        request.end();
      } catch (error) {
        console.error(`Error in calculate-cells route: ${error.message}`);

        return next(error);
      }
    },
    errors(),
    routeJoiErrorHandler,
  );
};
