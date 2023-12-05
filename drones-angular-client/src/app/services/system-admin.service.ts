import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { API_ROUTES } from 'src/api.config';
import { CreateUserCredentials } from '../components/create-user/create-user.component';
import { AuthResponse } from '../core/authentication/models/authResponse.model';
import Room from '../core/models/room.model';

@Injectable({
  providedIn: 'root',
})
export class SystemAdminService {
  constructor(private http: HttpClient) {}

  createUser(credentials: CreateUserCredentials): Observable<AuthResponse> {
    const route = API_ROUTES.user.register;
    return this.http.post<AuthResponse>(route, { user: credentials });
  }
}
