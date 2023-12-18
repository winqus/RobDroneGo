import { HTTP_INTERCEPTORS, HttpEvent, HttpInterceptor, HttpRequest, HttpResponse } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { set } from 'lodash';
import { Observable, of } from 'rxjs';
import { delay } from 'rxjs/operators';
import { UserRole } from '../authentication/models/user-roles.enum';
import { User } from '../authentication/models/user.model';
import { AuthMap } from '../mappers/auth.mapper';

const MOCK_JWT_TOKEN = 'mock-jwt-token';
const MOCK_JWT_TOKEN_KEY = 'JWT_TOKEN';
const MOCK_USER_STORAGE_KEY = 'mockUser';

@Injectable()
export class MockAuthHttpInterceptor implements HttpInterceptor {
  setCurrentMockUser = (user: User) => {
    window.localStorage.setItem(MOCK_USER_STORAGE_KEY, JSON.stringify(user));
  };

  getCurrentMockUser = (): User => {
    const mockUserJSONstring = window.localStorage.getItem(MOCK_USER_STORAGE_KEY);
    if (mockUserJSONstring) {
      const mockUser = JSON.parse(mockUserJSONstring);
      return mockUser;
    }

    return null as any;
  };

  removeCurrentMockUser = () => {
    window.localStorage.removeItem(MOCK_USER_STORAGE_KEY);
    window.localStorage.removeItem(MOCK_JWT_TOKEN_KEY);
  };

  intercept(request: HttpRequest<any>, next: any): Observable<HttpEvent<any>> {
    // Mock the getCurrentUser request
    if (request.url.endsWith('/api/auth/me') && request.method === 'GET') {
      console.log('Mocking the getCurrentUser request');
      const returnedUser = this.getCurrentMockUser();

      return of(new HttpResponse({ status: 200, body: { user: returnedUser } })).pipe(delay(500));
    }

    // Mock the login request
    if (request.url.endsWith('/api/auth/signin') && request.method === 'POST') {
      console.log('Mocking the login request');
      const loginCredentials = AuthMap.toLoginCredentials(request.body.user);
      this.setCurrentMockUser({
        id: 'random-id-132',
        firstName: 'John-user',
        lastName: 'Doemocker',
        email: loginCredentials.email,
        phonenumber: '0376372678',
        taxpayernumber: '726453678',
        role: UserRole.User,
        isConfirmed: true,
      });
      const returnedUser = this.getCurrentMockUser();

      return of(new HttpResponse({ status: 200, body: { user: returnedUser, token: MOCK_JWT_TOKEN } })).pipe(delay(500));
    }

    // Mock the register request
    if (request.url.endsWith('/api/auth/signup') && request.method === 'POST') {
      console.log('Mocking the register request');
      const requestedUser = request.body.user as User;
      this.setCurrentMockUser(requestedUser);
      const returnedUser = this.getCurrentMockUser();

      return of(new HttpResponse({ status: 200, body: { user: returnedUser, token: MOCK_JWT_TOKEN } })).pipe(delay(500));
    }

    // Mock the update user request (not implemented in backend)
    if (request.url.includes('/api/auth/user') && request.method === 'PUT') {
      console.log('Mocking the update user request');
      const requestedUser = request.body.user as User;
      this.setCurrentMockUser(requestedUser);
      const returnedUser = this.getCurrentMockUser();

      return of(new HttpResponse({ status: 200, body: { user: { ...returnedUser, ...request.body.user } } })).pipe(delay(500));
    }

    // Mock the logout request
    if (request.url.endsWith('/api/auth/logout') && request.method === 'POST') {
      console.log('Mocking the logout request');
      this.removeCurrentMockUser();

      return of(new HttpResponse({ status: 200 })).pipe(delay(500));
    }

    // For other unmocked requests, just pass them through
    return next.handle(request);
  }
}

export const mockAuthHttpInterceptorProvider = {
  provide: HTTP_INTERCEPTORS,
  useClass: MockAuthHttpInterceptor,
  multi: true,
};
