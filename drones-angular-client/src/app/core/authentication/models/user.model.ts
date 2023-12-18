import { UserRole } from './user-roles.enum';

export interface User {
  id: string;
  firstName: string;
  lastName: string;
  email: string;
  phonenumber: string;
  taxpayernumber: string;
  role: UserRole;
  isConfirmed: boolean;
}
