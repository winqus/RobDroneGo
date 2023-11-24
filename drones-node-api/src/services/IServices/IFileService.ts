import { Result } from '../../core/logic/Result';

export default interface IFileService {
  downloadFile(file: string): Promise<Result<string>>;
  uploadFile(file: any): Promise<Result<boolean>>;
  listAllFiles(): Promise<Result<object[]>>;
}
