import { Container } from 'typedi';

import { Mapper } from '../core/infra/Mapper';

import { IUserDTO } from '../dto/IUserDTO';

import { UniqueEntityID } from '../core/domain/UniqueEntityID';
import { User } from '../domain/user';

import { UserEmail } from '../domain/userEmail';
import { UserPassword } from '../domain/userPassword';

import { IUserPersistence } from '../dataschema/IUserPersistence';
import { Role } from '../domain/role';
import { UserRole } from '../domain/userRole.enum';
import RoleRepo from '../repos/roleRepo';

export class UserMap extends Mapper<User> {
  public static toDTO(user: User): IUserDTO {
    return {
      id: user.id.toString(),
      firstName: user.firstName,
      lastName: user.lastName,
      email: user.email.value,
      phonenumber: user.phonenumber,
      taxpayernumber: user.taxpayernumber,
      password: '',
      role: user.role.id.toString() as UserRole,
      isConfirmed: user.isConfirmed,
    };
  }

  public static async toDomain(raw: any): Promise<User> {
    const userEmailOrError = UserEmail.create(raw.email);
    const userPasswordOrError = UserPassword.create({ value: raw.password, hashed: true });
    const role = Role.create(
      { name: raw.role, id: raw.roleId || raw.role },
      new UniqueEntityID(raw.roleId || raw.role),
    );

    const userOrError = User.create(
      {
        firstName: raw.firstName,
        lastName: raw.lastName,
        email: userEmailOrError.getValue(),
        password: userPasswordOrError.getValue(),
        phonenumber: raw.phonenumber,
        taxpayernumber: raw.taxpayernumber,
        role: role.getValue(),
        isConfirmed: raw.isConfirmed,
      },
      new UniqueEntityID(raw.domainId || raw.id),
    );

    userOrError.isFailure ? console.log(userOrError.error) : '';

    return userOrError.isSuccess ? userOrError.getValue() : null;
  }

  public static toPersistence(user: User): IUserPersistence {
    const userPersistence: IUserPersistence = {
      domainId: user.id.toString(),
      email: user.email.value,
      password: user.password.value,
      firstName: user.firstName,
      lastName: user.lastName,
      phonenumber: user.phonenumber,
      taxpayernumber: user.taxpayernumber,
      role: user.role.id.toString() as UserRole,
      isConfirmed: user.isConfirmed,
      salt: '',
    };

    return userPersistence;
  }
}
