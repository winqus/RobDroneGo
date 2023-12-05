import { UserRole } from './user-roles.enum';

export interface User {
  firstName: string;
  lastName: string;
  email: string;
  phonenumber: string;
  taxpayernumber: string;
  role: UserRole;
}
