import { UserRole } from "./user-roles.enum";

export interface User {
  firstName: string;
  lastName: string;
  email: string;
  role: UserRole;
}
