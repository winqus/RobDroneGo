import { AggregateRoot } from '../core/domain/AggregateRoot';
import { UniqueEntityID } from '../core/domain/UniqueEntityID';

import { Result } from '../core/logic/Result';
import { RoleId } from './roleId';

import IRoleDTO from '../dto/IRoleDTO';
import { UserRole } from './userRole.enum';

interface RoleProps {
  name: UserRole;
}

function isValidUserRole(name: string): name is UserRole {
  return Object.values(UserRole).includes(name as UserRole);
}

export class Role extends AggregateRoot<RoleProps> {
  get id(): UniqueEntityID {
    return this._id;
  }

  get roleId(): RoleId {
    return new RoleId(this.roleId.toValue());
  }

  get name(): UserRole {
    return this.props.name;
  }

  set name(value: UserRole) {
    this.props.name = value;
  }

  private constructor(props: RoleProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create(roleDTO: IRoleDTO, id?: UniqueEntityID): Result<Role> {
    const name = roleDTO.name;

    if (!name || !isValidUserRole(name)) {
      return Result.fail<Role>('Must provide a proper role name');
    } else {
      const role = new Role({ name: name }, id);

      return Result.ok<Role>(role);
    }
  }
}
