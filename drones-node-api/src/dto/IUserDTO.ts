import { UserRole } from '../domain/userRole.enum';

export interface IUserDTO {
  id: string;
  firstName: string;
  lastName: string;
  email: string;
  phonenumber: string;
  taxpayernumber: string;
  password: string;
  role: UserRole;
  isConfirmed: boolean;
}
