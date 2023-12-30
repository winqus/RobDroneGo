import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { API_ROUTES } from '../../../../api.config';
import { AuthResponse } from '../models/authResponse.model';
import LoginCredentials from '../models/loginCredentials.model';
import RegisterCredentials from '../models/registerCredentials.model';
import { UserRole } from '../models/user-roles.enum';
import { User } from '../models/user.model';
import { JwtService } from './jwt.service';
import { UserService } from './user.service';

describe('UserService', () => {
  let userService: UserService;
  let httpMock: HttpTestingController;
  let jwtServiceSpy: jasmine.SpyObj<JwtService>;

  let userStub: User;

  beforeEach(() => {
    userStub = {
      id: '1',
      firstName: 'John',
      lastName: 'Doe',
      email: 'john@mail.mock',
      phonenumber: '123456789',
      taxpayernumber: '123456789',
      role: UserRole.User,
      isConfirmed: true,
    };

    const spy = jasmine.createSpyObj('JwtService', ['saveToken', 'clearToken']);

    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule, RouterTestingModule],
      providers: [UserService, { provide: JwtService, useValue: spy }],
    });

    userService = TestBed.inject(UserService);
    httpMock = TestBed.inject(HttpTestingController);
    jwtServiceSpy = TestBed.inject(JwtService) as jasmine.SpyObj<JwtService>;
  });

  afterEach(() => {
    httpMock.verify();
  });

  it('should be created', () => {
    expect(userService).toBeTruthy();
  });

  it('should fetch the current user', () => {
    const mockUser: User = userStub;
    userService.getCurrentUser().subscribe((user) => {
      expect(user).toEqual({ user: mockUser });
    });

    const req = httpMock.expectOne(API_ROUTES.user.me);
    expect(req.request.method).toBe('GET');
    req.flush({ user: mockUser });
  });

  it('should login', () => {
    const fakeUser: User = userStub;
    const loginCredentials: LoginCredentials = {
      email: fakeUser.email,
      password: '123456789',
    };

    const fakeAuthResponse: AuthResponse = {
      user: fakeUser,
      token: 'sometoken',
    };

    userService.login(loginCredentials).subscribe((user) => {
      expect(user).toEqual(fakeAuthResponse);
    });

    const req = httpMock.expectOne(API_ROUTES.user.login);
    expect(req.request.method).toBe('POST');
    req.flush(fakeAuthResponse);
  });

  it('should register', () => {
    const fakeUser: User = userStub;
    const registerCredentials: RegisterCredentials = {
      firstName: fakeUser.firstName,
      lastName: fakeUser.lastName,
      email: fakeUser.email,
      phonenumber: fakeUser.phonenumber,
      taxpayernumber: fakeUser.taxpayernumber,
      role: fakeUser.role,
      password: '123456789',
    };

    const fakeAuthResponse: AuthResponse = {
      user: fakeUser,
      token: 'sometoken',
    };

    userService.register(registerCredentials).subscribe((user) => {
      expect(user).toEqual(fakeAuthResponse);
    });

    const req = httpMock.expectOne(API_ROUTES.user.register);
    expect(req.request.method).toBe('POST');
    req.flush(fakeAuthResponse);
  });

  it('should update', () => {
    const fakeUser: User = userStub;
    const partialUser: Partial<User> = {
      firstName: fakeUser.firstName,
      lastName: fakeUser.lastName,
      email: fakeUser.email,
      phonenumber: fakeUser.phonenumber,
      taxpayernumber: fakeUser.taxpayernumber,
      role: fakeUser.role,
    };

    const fakeAuthResponse: AuthResponse = {
      user: fakeUser,
      token: 'sometoken',
    };

    userService.update(partialUser).subscribe((user) => {
      expect(user).toEqual(fakeAuthResponse);
    });

    const req = httpMock.expectOne(API_ROUTES.user.update);
    expect(req.request.method).toBe('PATCH');
    req.flush(fakeAuthResponse);
  });

  it('should perform logout', () => {
    userService.logout();
    expect(jwtServiceSpy.clearToken).toHaveBeenCalled();
  });

  it('should set authentication', () => {
    const fakeUser: User = userStub;
    const fakeToken: string = 'sometoken';

    userService.setAuthentication(fakeUser, fakeToken);
    expect(jwtServiceSpy.saveToken).toHaveBeenCalled();
  });

  it('should purge authentication', () => {
    userService.purgeAuthentication();
    expect(jwtServiceSpy.clearToken).toHaveBeenCalled();
  });

  it('should return true if user has role', () => {
    const fakeUser: User = userStub;
    const allowedRoles: UserRole[] = [UserRole.User];

    userService.setAuthentication(fakeUser, 'sometoken');
    expect(userService.hasRole(allowedRoles)).toBeTrue();
  });

  it('should get all users', () => {
    const fakeUser: User = userStub;
    const fakeUsers: User[] = [fakeUser, fakeUser, fakeUser];

    userService.getAllUsers().subscribe((users) => {
      expect(users).toEqual(fakeUsers);
    });

    const req = httpMock.expectOne(API_ROUTES.user.getAll);
    expect(req.request.method).toBe('GET');
    req.flush(fakeUsers);
  });

  it('should confirm user', () => {
    const fakeUser: User = userStub;
    const fakeUsers: User[] = [fakeUser, fakeUser, fakeUser];

    userService.confirmUser(fakeUser.email, true).subscribe((users) => {
      expect(users).toEqual(fakeUsers);
    });

    const req = httpMock.expectOne(API_ROUTES.user.confirm);
    expect(req.request.method).toBe('PATCH');
    req.flush(fakeUsers);
  });

  it('should delete user', () => {
    const fakeUser: User = userStub;
    const fakeUsers: User[] = [fakeUser, fakeUser, fakeUser];

    userService.deleteSelf().subscribe((users) => {
      expect(users).toEqual(fakeUsers);
    });

    const req = httpMock.expectOne(API_ROUTES.user.delete);
    expect(req.request.method).toBe('DELETE');
    req.flush(fakeUsers);
  });
});
