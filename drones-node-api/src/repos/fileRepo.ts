import e from 'express';
import { Service } from 'typedi';
import config from '../../config';
import IFileRepo from '../services/IRepos/IFileRepo';
const fs = require('fs');

@Service()
export default class FileRepo implements IFileRepo {
  public async uploadFile(file: any): Promise<boolean> {
    const filePath = config.base_dir + `${config.publicFolder}/${file.originalname}`;

    if (this.exists(filePath)) {
      return false;
    }

    return true;
  }

  public async downloadFile(file: string): Promise<string> {
    const filePath = config.base_dir + `${config.publicFolder}/${file}`;

    if (this.exists(filePath)) {
      return filePath;
    }

    return null;
  }

  exists(filePath: string): Promise<boolean> {
    return new Promise((resolve, reject) => {
      fs.access(filePath, fs.constants.F_OK, (err) => {
        if (err) {
          // File does not exist or cannot be accessed
          resolve(false);
        } else {
          // File exists
          resolve(true);
        }
      });
    });
  }

  save(t: string): Promise<any> {
    throw new Error('Method not implemented.');
  }

  public async listAllFiles(): Promise<string[]> {
    try {
      const files = fs.readdirSync(config.base_dir + config.publicFolder);

      return files;
    } catch (error) {
      fs.mkdirSync(global.__basedir + config.publicFolder);

      return [];
    }
  }
}
