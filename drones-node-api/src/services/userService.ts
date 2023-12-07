import { Inject, Service } from 'typedi';

import argon2 from 'argon2';
import { randomBytes } from 'crypto';
import jwt from 'jsonwebtoken';
import config from '../../config';

//import MailerService from './mailer.ts.bak';

import { IUserDTO } from '../dto/IUserDTO';
import { UserMap } from '../mappers/UserMap';
import IUserService, { UserToken } from '../services/IServices/IUserService';

import IRoleRepo from './IRepos/IRoleRepo';
import IUserRepo from './IRepos/IUserRepo';

import { User } from '../domain/user';
import { UserEmail } from '../domain/userEmail';
import { UserPassword } from '../domain/userPassword';

import { Role } from '../domain/role';

import { Result } from '../core/logic/Result';

@Service()
export default class UserService implements IUserService {
  constructor(
    @Inject(config.repos.user.name) private userRepo: IUserRepo,
    @Inject(config.repos.role.name) private roleRepo: IRoleRepo,
    @Inject('logger') private logger,
  ) {}

  public async getUserByEmail(email: string): Promise<Result<IUserDTO>> {
    try {
      const user = await this.userRepo.findByEmail(email);
      const found = !!user;

      if (!found) {
        return Result.fail<IUserDTO>('User not found with email=' + email);
      }

      const userDTO = UserMap.toDTO(user) as IUserDTO;

      return Result.ok<IUserDTO>(userDTO);
    } catch (e) {
      this.logger.error(e);
      throw e;
    }
  }

  public async getUserById(id: string): Promise<Result<IUserDTO>> {
    try {
      const user = await this.userRepo.findById(id);
      const found = !!user;

      if (!found) {
        return Result.fail<IUserDTO>('User not found with id=' + id);
      }

      const userDTO = UserMap.toDTO(user) as IUserDTO;

      return Result.ok<IUserDTO>(userDTO);
    } catch (e) {
      this.logger.error(e);
      throw e;
    }
  }

  public async signUp(userDTO: IUserDTO): Promise<Result<{ userDTO: IUserDTO }>> {
    try {
      const userDocument = await this.userRepo.findByEmail(userDTO.email);
      const found = !!userDocument;

      if (found) {
        return Result.fail<{ userDTO: IUserDTO }>({ message: 'User already exists with email=' + userDTO.email });
      }

      if (config.allowedEmailDomains?.length > 0 && !config.allowedEmailDomains.includes(userDTO.email.split('@')[1])) {
        return Result.fail<{ userDTO: IUserDTO }>({ message: 'Email must end with a permitted domain' });
      }

      const salt = randomBytes(32);
      this.logger.silly('Hashing password');
      const hashedPassword = await argon2.hash(userDTO.password, { salt });
      this.logger.silly('Creating user db record');

      const password = await UserPassword.create({ value: hashedPassword, hashed: true }).getValue();
      const email = await UserEmail.create(userDTO.email).getValue();
      let role: Role;

      const roleOrError = await this.getRole(userDTO.role || config.defaultUserRole);
      if (roleOrError.isFailure) {
        return Result.fail<{ userDTO: IUserDTO }>(roleOrError.error);
      } else {
        role = roleOrError.getValue();
      }

      const userOrError = await User.create({
        firstName: userDTO.firstName,
        lastName: userDTO.lastName,
        email: email,
        phonenumber: userDTO.phonenumber,
        taxpayernumber: userDTO.taxpayernumber || '',
        role: role,
        password: password,
        isConfirmed: userDTO.isConfirmed || false,
      });

      if (userOrError.isFailure) {
        throw Result.fail<{ userDTO: IUserDTO }>(userOrError.errorValue());
      }

      const userResult = userOrError.getValue();

      // this.logger.silly('Generating JWT');
      // const token = this.generateToken(userResult);

      // this.logger.silly('Sending welcome email');
      //await this.mailer.SendWelcomeEmail(userResult);

      //this.eventDispatcher.dispatch(events.user.signUp, { user: userResult });

      await this.userRepo.save(userResult);
      const userDTOResult = UserMap.toDTO(userResult) as IUserDTO;

      return Result.ok<{ userDTO: IUserDTO }>({ userDTO: userDTOResult });
    } catch (e) {
      this.logger.error(e);
      throw e;
    }
  }

  public async signIn(email: string, password: string): Promise<Result<UserToken>> {
    const user = await this.userRepo.findByEmail(email);

    if (!user) {
      return Result.fail<UserToken>('Invalid Email or Password');
    }

    /**
     * We use verify from argon2 to prevent 'timing based' attacks
     */
    this.logger.silly('Checking password');
    const validPassword = await argon2.verify(user.password.value, password);
    if (validPassword) {
      this.logger.silly('Password is valid!');
      if (user.isConfirmed === false) {
        return Result.fail<UserToken>({ message: 'User not confirmed' });
      }

      this.logger.silly('Generating JWT');
      const token = this.generateToken(user) as string;

      const userDTO = UserMap.toDTO(user) as IUserDTO;

      return Result.ok<UserToken>({ userDTO: userDTO, token: token });
    } else {
      return Result.fail<UserToken>('Invalid Email or Password');
    }
  }

  public async updateUser(email: string, userDTO: Partial<IUserDTO>): Promise<Result<UserToken>> {
    try {
      const user = await this.userRepo.findByEmail(email);
      const found = !!user;

      if (!found) {
        return Result.fail<UserToken>('User not found with email=' + userDTO.email);
      }

      if (userDTO.email && email !== userDTO.email) {
        const userWithNewEmail = await this.userRepo.findByEmail(userDTO.email);

        if (userWithNewEmail) {
          return Result.fail('Email taken');
        }

        if (
          config.allowedEmailDomains?.length > 0 &&
          !config.allowedEmailDomains.includes(userDTO.email.split('@')[1])
        ) {
          return Result.fail({ message: 'Email must end with a permitted domain' });
        }
      }

      if (userDTO.firstName) {
        user.firstName = userDTO.firstName;
      }
      if (userDTO.lastName) {
        user.lastName = userDTO.lastName;
      }
      if (userDTO.email) {
        user.email = await UserEmail.create(userDTO.email).getValue();
      }
      if (userDTO.phonenumber) {
        user.phonenumber = userDTO.phonenumber;
      }
      if (userDTO.taxpayernumber) {
        user.taxpayernumber = userDTO.taxpayernumber;
      }
      if (userDTO.password) {
        const salt = randomBytes(32);
        const hashedPassword = await argon2.hash(userDTO.password, { salt });
        user.password = await UserPassword.create({ value: hashedPassword, hashed: true }).getValue();
      }

      const updatedUser = await this.userRepo.save(user);
      const updatedUserDTO = UserMap.toDTO(updatedUser) as IUserDTO;

      const token = this.generateToken(updatedUser) as string;

      return Result.ok<UserToken>({ userDTO: updatedUserDTO, token: token });
    } catch (e) {
      this.logger.error(e);
      throw e;
    }
  }

  public async setUserConfirmation(email: string, isConfirmed: boolean): Promise<Result<void>> {
    try {
      const user = await this.userRepo.findByEmail(email);
      const found = !!user;

      if (!found) {
        return Result.fail<void>('User not found with email=' + email);
      }

      user.isConfirmed = isConfirmed;

      await this.userRepo.save(user);

      return Result.ok<void>();
    } catch (e) {
      this.logger.error(e);
      throw e;
    }
  }

  public async getAllUsers(): Promise<Result<IUserDTO[]>> {
    try {
      const users = await this.userRepo.getAll();

      const userDTOs = users.map((user) => UserMap.toDTO(user) as IUserDTO);

      return Result.ok<IUserDTO[]>(userDTOs);
    } catch (e) {
      this.logger.error(e);
      throw e;
    }
  }

  public async deleteUser(email: string): Promise<Result<void>> {
    try {
      const user = await this.userRepo.findByEmail(email);
      const found = !!user;

      if (!found) {
        return Result.fail<void>('User not found with email=' + email);
      }

      await this.userRepo.delete(user);

      return Result.ok<void>();
    } catch (e) {
      this.logger.error(e);
      throw e;
    }
  }

  private generateToken(user) {
    const today = new Date();
    const exp = new Date(today);
    exp.setDate(today.getDate() + 60); // Token will last 60 days

    this.logger.silly(`Sign JWT for userId: ${user._id}`);

    const id = user.id.toString();
    const email = user.email.value;
    const firstName = user.firstName;
    const lastName = user.lastName;
    const role = user.role.id.value;

    return jwt.sign(
      {
        id: id,
        email: email, // We are gonna use this in the middleware 'isAuth'
        role: role,
        firstName: firstName,
        lastName: lastName,
        phonenumber: user.phonenumber,
        taxpayernumber: user.taxpayernumber,
        exp: exp.getTime() / 1000,
      } as IUserDTO & { exp: number },
      config.jwtSecret,
    );
  }

  private async getRole(roleId: string): Promise<Result<Role>> {
    const role = await this.roleRepo.findByDomainId(roleId);
    const found = !!role;

    if (found) {
      return Result.ok<Role>(role);
    } else {
      return Result.fail<Role>("Couldn't find role by id=" + roleId);
    }
  }
}
