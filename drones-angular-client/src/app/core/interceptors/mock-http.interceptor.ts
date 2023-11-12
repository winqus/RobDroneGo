import { HTTP_INTERCEPTORS, HttpEvent, HttpInterceptor, HttpRequest, HttpResponse } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable, of } from 'rxjs';
import { delay } from 'rxjs/operators';
import { UserRole } from '../authentication/models/user-roles.enum';
import { User } from '../authentication/models/user.model';

const MOCK_USERS = {
  'user': {
    firstName: 'John-user',
    lastName: 'Doemocker',
    email: 'john.user@mock-http-intercept.com',
    role: UserRole.User,
  } as User,
  'system': {
    firstName: 'Joe-admin',
    lastName: 'Doe-admin',
    email: 'joe.admin@mock-http-intercept.com',
    role: UserRole.SystemAdministrator,
  } as User,
  'fleet': {
    firstName: 'Jane-fleet',
    lastName: 'Doe-fleet',
    email: 'Jane.fleet@mock-http-intercept.com',
    role: UserRole.FleetManager,
  } as User,
  'campus': {
    firstName: 'Jack-campus',
    lastName: 'Doe-campus',
    email: 'Jack.campus@mock-http-intercept.com',
    role: UserRole.CampusManager,
  } as User,
  'task': {
    firstName: 'Jill-task',
    lastName: 'Doe-task',
    email: 'Jill.task@mock-http-intercept.com',
    role: UserRole.TaskManager,
  } as User,
} as { [key: string]: User };

const DEFAULT_MOCK_USER: User = MOCK_USERS['user'];

const getUserMockByRole = (role: string): User => MOCK_USERS[role] || DEFAULT_MOCK_USER;

function findMockUserByEmail(email: string): User {
  for (const key in MOCK_USERS) {
    if (email.includes(key)) {
      return MOCK_USERS[key];
    }
  }
  return DEFAULT_MOCK_USER;
}

const setCurrentMockUser = (user: User) => {
  CURRENT_MOCK_USER = user;
  window.localStorage.setItem('mockUser', JSON.stringify(user));
}

const getCurrentMockUser = (): User => {
  const mockUser = window.localStorage.getItem('mockUser');
  if (mockUser) {
    CURRENT_MOCK_USER = JSON.parse(mockUser);
  }
  else {
    CURRENT_MOCK_USER = DEFAULT_MOCK_USER;
  }
  return CURRENT_MOCK_USER;
}

let CURRENT_MOCK_USER: User = DEFAULT_MOCK_USER;

const MOCK_TOKEN = 'mock-jwt-token';


@Injectable()
export class MockHttpInterceptor implements HttpInterceptor {

  intercept(request: HttpRequest<any>, next: any): Observable<HttpEvent<any>> {
    // Mock the getCurrentUser request
    if (request.url.endsWith('/api/auth/me') && request.method === 'GET') {
      console.log('Mocking the getCurrentUser request');
      getCurrentMockUser();
      return of(new HttpResponse({ status: 200, body: { user: CURRENT_MOCK_USER } })).pipe(delay(500));
    }

    // Mock the login request
    if (request.url.endsWith('/api/auth/signin') && request.method === 'POST') {
      console.log('Mocking the login request');
      const requestedUser = request.body.user as User;
      setCurrentMockUser(findMockUserByEmail(requestedUser.email));
      return of(new HttpResponse({ status: 200, body: { user: CURRENT_MOCK_USER, token: MOCK_TOKEN } })).pipe(delay(500));
    }

    // Mock the register request
    if (request.url.endsWith('/api/auth/signup') && request.method === 'POST') {
      console.log('Mocking the register request');
      const requestedUser = request.body.user as User;
      setCurrentMockUser(findMockUserByEmail(requestedUser.email));
      return of(new HttpResponse({ status: 200, body: { user: CURRENT_MOCK_USER, token: MOCK_TOKEN } })).pipe(delay(500));
    }

    // Mock the update user request (not implemented in backend)
    if (request.url.includes('/api/auth/user/') && request.method === 'PUT') {
      console.log('Mocking the update user request');
      getCurrentMockUser();
      return of(new HttpResponse({ status: 200, body: { user: { ...CURRENT_MOCK_USER, ...request.body.user } } })).pipe(delay(500));
    }

    // Mock the logout request
    if (request.url.endsWith('/api/auth/logout') && request.method === 'POST') {
      console.log('Mocking the logout request');
      return of(new HttpResponse({ status: 200 })).pipe(delay(500));
    }

    // For other unmocked requests, just pass them through
    return next.handle(request);
  }
}

export const mockHttpInterceptorProvider = {
  provide: HTTP_INTERCEPTORS,
  useClass: MockHttpInterceptor,
  multi: true
};
