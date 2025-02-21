import 'reflect-metadata'; // We need this in order to use @Decorators

import config from '../config';

import express from 'express';

import Logger from './loaders/logger';

async function startServer() {
  const app = express();

  await require('./loaders').default({ expressApp: app });

  app
    .listen(config.port, () => {
      console.log('Server listening on port: ' + config.port);

      Logger.info(`
      ################################################
      🛡️  Server listening on port: ${config.port} 🛡️ 
      DB: ${['test', 'development'].some((x) => x === process.env.NODE_ENV) ? config.databaseURL : '******'}
      ENV: ${process.env.NODE_ENV}
      ################################################
    `);
    })
    .on('error', (err) => {
      Logger.error(err);

      return;
    });
}

startServer();
