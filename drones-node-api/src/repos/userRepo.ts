import { Inject, Service } from 'typedi';

import { Document, Model } from 'mongoose';
import { IUserPersistence } from '../dataschema/IUserPersistence';

import { User } from '../domain/user';
import { UserEmail } from '../domain/userEmail';
import { UserId } from '../domain/userId';
import { UserMap } from '../mappers/UserMap';
import IUserRepo from '../services/IRepos/IUserRepo';

@Service()
export default class UserRepo implements IUserRepo {
  private models: any;

  constructor(
    @Inject('userSchema') private userSchema: Model<IUserPersistence & Document>,
    @Inject('logger') private logger,
  ) {}

  private createBaseQuery(): any {
    return {
      where: {},
    };
  }

  public async exists(userId: UserId | string): Promise<boolean> {
    const idX = userId instanceof UserId ? (<UserId>userId).id.toValue() : userId;

    const query = { domainId: idX };
    const userDocument = await this.userSchema.findOne(query);

    return !!userDocument === true;
  }

  public async save(user: User): Promise<User> {
    const query = { domainId: user.id.toString() };

    const userDocument = await this.userSchema.findOne(query);

    try {
      if (userDocument === null) {
        const rawUser: any = UserMap.toPersistence(user);

        const userCreated = await this.userSchema.create(rawUser);

        return UserMap.toDomain(userCreated);
      } else {
        userDocument.firstName = user.firstName;
        userDocument.lastName = user.lastName;
        userDocument.email = user.email.value;
        userDocument.phonenumber = user.phonenumber;
        userDocument.taxpayernumber = user.taxpayernumber;
        userDocument.isConfirmed = user.isConfirmed;
        await userDocument.save();

        return user;
      }
    } catch (err) {
      throw err;
    }
  }

  public async findByEmail(email: UserEmail | string): Promise<User> {
    const query = { email: email.toString() };
    const userRecord = await this.userSchema.findOne(query);

    if (userRecord != null) {
      return UserMap.toDomain(userRecord);
    } else {
      return null;
    }
  }

  public async findById(userId: UserId | string): Promise<User> {
    const idX = userId instanceof UserId ? (<UserId>userId).id.toValue() : userId;

    const query = { domainId: idX };
    const userRecord = await this.userSchema.findOne(query);

    if (userRecord != null) {
      return UserMap.toDomain(userRecord);
    } else {
      return null;
    }
  }

  public async getAll(): Promise<User[]> {
    const users = await this.userSchema.find();

    const usersDomain = await Promise.all(users.map(async (user) => await UserMap.toDomain(user)));

    return usersDomain;
  }

  public async delete(user: User): Promise<boolean> {
    const query = { domainId: user.id.toString() };
    await this.userSchema.deleteOne(query);

    return true;
  }
}
