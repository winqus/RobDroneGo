import { UserRole } from '../domain/userRole.enum';

export default interface IRoleDTO {
  id: string;
  name: UserRole;
}
