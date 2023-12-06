import { Result } from '../../core/logic/Result';
import { IUserDTO } from '../../dto/IUserDTO';

export interface UserToken {
  userDTO: IUserDTO;
  token: string;
}

export default interface IUserService {
  signUp(userDTO: IUserDTO): Promise<Result<{ userDTO: IUserDTO }>>;
  signIn(email: string, password: string): Promise<Result<UserToken>>;
  updateUser(email: string, userDTO: Partial<IUserDTO>): Promise<Result<UserToken>>;
  setUserConfirmation(email: string, isConfirmed: boolean): Promise<Result<void>>;
  getUserByEmail(email: string): Promise<Result<IUserDTO>>;
  getUserById(id: string): Promise<Result<IUserDTO>>;
  getAllUsers(): Promise<Result<IUserDTO[]>>;
  deleteUser(email: string): Promise<Result<void>>;
}
