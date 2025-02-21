import { Repo } from '../../core/infra/Repo';

export default interface IFileRepo extends Repo<string> {
  listAllFiles(): Promise<string[]>;
  uploadFile(file: string): Promise<boolean>;
  downloadFile(fileId: string): Promise<string>;
}
