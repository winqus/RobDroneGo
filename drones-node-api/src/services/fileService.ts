import { Inject, Service } from 'typedi';

import config from '../../config';
import { Result } from '../core/logic/Result';
import IFileRepo from './IRepos/IFileRepo';
import IFileService from './IServices/IFileService';

@Service()
export default class FileService implements IFileService {
  constructor(@Inject(config.repos.file.name) private fileRepo: IFileRepo) {}

  public async uploadFile(file: any): Promise<Result<boolean>> {
    if (this.fileRepo.uploadFile(file)) {
      return Result.ok<boolean>(true);
    } else {
      return Result.fail<boolean>('Upload failed');
    }
  }

  public async downloadFile(file: string): Promise<Result<string>> {
    const path = await this.fileRepo.downloadFile(file);
    if (path) {
      return Result.ok<string>(path);
    } else {
      return Result.fail<string>('Download failed');
    }
  }

  public async listAllFiles(): Promise<Result<object[]>> {
    const filesNames = await this.fileRepo.listAllFiles();
    if (filesNames) {
      const files = [];
      filesNames.forEach((file) => {
        files.push({ name: file, path: '/folder/download/' + file });
      });

      return Result.ok<object[]>(files);
    } else {
      return Result.fail<object[]>('List files failed');
    }
  }
}
