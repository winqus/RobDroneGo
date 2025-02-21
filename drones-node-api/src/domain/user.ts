import { AggregateRoot } from '../core/domain/AggregateRoot';
import { UniqueEntityID } from '../core/domain/UniqueEntityID';
import { Guard } from '../core/logic/Guard';
import { Result } from '../core/logic/Result';
import { Role } from '../domain/role';
import { UserEmail } from './userEmail';
import { UserId } from './userId';
import { UserPassword } from './userPassword';

interface UserProps {
  firstName: string;
  lastName: string;
  email: UserEmail;
  phonenumber: string;
  taxpayernumber: string;
  password: UserPassword;
  role: Role;
  isConfirmed?: boolean;
}

export class User extends AggregateRoot<UserProps> {
  get id(): UniqueEntityID {
    return this._id;
  }

  get userId(): UserId {
    return UserId.caller(this.id);
  }

  get email(): UserEmail {
    return this.props.email;
  }

  set email(email: UserEmail) {
    this.props.email = email;
  }

  get firstName(): string {
    return this.props.firstName;
  }

  set firstName(value: string) {
    this.props.firstName = value;
  }

  get lastName(): string {
    return this.props.lastName;
  }

  set lastName(value: string) {
    this.props.lastName = value;
  }

  get phonenumber(): string {
    return this.props.phonenumber;
  }

  set phonenumber(value: string) {
    this.props.phonenumber = value;
  }

  get taxpayernumber(): string {
    return this.props.taxpayernumber;
  }

  set taxpayernumber(value: string) {
    this.props.taxpayernumber = value;
  }

  get password(): UserPassword {
    return this.props.password;
  }

  set password(value: UserPassword) {
    this.props.password = value;
  }

  get role(): Role {
    return this.props.role;
  }

  set role(value: Role) {
    this.props.role = value;
  }

  get isConfirmed(): boolean {
    return this.props.isConfirmed;
  }

  set isConfirmed(value: boolean) {
    this.props.isConfirmed = value;
  }

  private constructor(props: UserProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create(props: UserProps, id?: UniqueEntityID): Result<User> {
    const guardedProps = [
      { argument: props.firstName, argumentName: 'firstName' },
      { argument: props.lastName, argumentName: 'lastName' },
      { argument: props.email, argumentName: 'email' },
      { argument: props.role, argumentName: 'role' },
      { argument: props.phonenumber, argumentName: 'phonenumber' },
      { argument: props.taxpayernumber, argumentName: 'taxpayernumber' },
    ];

    const guardResult = Guard.againstNullOrUndefinedBulk(guardedProps);

    if (!guardResult.succeeded) {
      return Result.fail<User>(guardResult.message);
    } else {
      const user = new User(
        {
          ...props,
        },
        id,
      );

      return Result.ok<User>(user);
    }
  }
}
