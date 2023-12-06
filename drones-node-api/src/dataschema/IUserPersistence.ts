export interface IUserPersistence {
  domainId: string;
  firstName: string;
  lastName: string;
  email: string;
  phonenumber: string;
  taxpayernumber: string;
  password: string;
  salt: string;
  role: string;
  isConfirmed: boolean;
}
