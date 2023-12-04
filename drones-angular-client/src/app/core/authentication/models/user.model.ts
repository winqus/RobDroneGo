import { UserRole } from './user-roles.enum';

export interface User {
  firstName: string;
  lastName: string;
  email: string;
  phonenumber: string;
  role: UserRole;
}
