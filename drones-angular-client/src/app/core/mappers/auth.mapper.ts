import { retryWhen } from 'rxjs';
import LoginCredentials from '../authentication/models/loginCredentials.model';
import RegisterCredentials from '../authentication/models/registerCredentials.model';

export class AuthMap {
  public static toRegisterCredentials(registerCredentials: any): RegisterCredentials {
    return {
      firstName: registerCredentials.firstName,
      lastName: registerCredentials.lastName,
      email: registerCredentials.email,
      password: registerCredentials.password,
      phonenumber: registerCredentials.phonenumber,
      taxpayernumber: registerCredentials.taxpayernumber,
      role: registerCredentials.role || registerCredentials.userRole,
    };
  }

  public static toLoginCredentials(loginCredentials: any): LoginCredentials {
    return {
      email: loginCredentials.email,
      password: loginCredentials.password,
    };
  }
}
