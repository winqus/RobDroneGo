import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Router } from '@angular/router';
import { BehaviorSubject, Observable } from 'rxjs';
import { distinctUntilChanged, map, shareReplay, tap } from 'rxjs/operators';
import { API_ROUTES } from 'src/api.config';
import { AuthResponse } from '../models/authResponse.model';
import LoginCredentials from '../models/loginCredentials.model';
import RegisterCredentials from '../models/registerCredentials.model';
import { UserRole } from '../models/user-roles.enum';
import { User } from '../models/user.model';
import { JwtService } from './jwt.service';

@Injectable({ providedIn: 'root' })
export class UserService {
  private currentUserSubject = new BehaviorSubject<User | null>(null);
  public currentUser = this.currentUserSubject.asObservable().pipe(distinctUntilChanged());

  public isAuthenticated: Observable<boolean> = this.currentUser.pipe(map((user) => Boolean(user)));

  constructor(
    private http: HttpClient,
    private jwtService: JwtService,
    private router: Router,
  ) {}

  getCurrentUser(): Observable<{ user: User }> {
    const route = API_ROUTES.user.me;
    return this.http.get<{ user: User }>(route).pipe(
      tap({
        next: ({ user }) => this.currentUserSubject.next(user),
        error: (error) => {
          this.purgeAuthentication();
          window.location.reload();
        },
      }),
      shareReplay(1),
    );
  }

  login(credentials: LoginCredentials): Observable<AuthResponse> {
    const route = API_ROUTES.user.login;
    return this.http.post<AuthResponse>(route, { ...credentials }).pipe(tap(({ user, token }) => this.setAuthentication(user, token)));
  }

  register(credentials: RegisterCredentials): Observable<AuthResponse> {
    const route = API_ROUTES.user.register;
    return this.http.post<AuthResponse>(route, { ...credentials });
  }

  update(user: Partial<User>): Observable<{ user: User }> {
    const route = API_ROUTES.user.update;
    return this.http.patch<AuthResponse>(route, { ...user }).pipe(
      tap(({ user, token }) => {
        this.setAuthentication(user, token);
      }),
    );
  }

  logout(): void {
    const route = API_ROUTES.user.logout;
    this.http.post(route, {});
    this.purgeAuthentication();
    this.router.navigate(['/']);
  }

  setAuthentication(user: User, token: string): void {
    this.jwtService.saveToken(token);
    this.currentUserSubject.next(user);
  }

  purgeAuthentication(): void {
    this.jwtService.clearToken();
    this.currentUserSubject.next(null);
  }

  hasRole(allowedRoles: UserRole[]): boolean {
    const currentUser = this.currentUserSubject.value;

    if (currentUser && allowedRoles.includes(currentUser.role)) {
      return true;
    }

    return false;
  }

  getAllUsers(): Observable<User[]> {
    const route = API_ROUTES.user.getAll;
    return this.http.get<User[]>(route);
  }

  confirmUser(email: string, confirmed: boolean) {
    const route = API_ROUTES.user.confirm;
    return this.http.patch(route, { email, confirmed });
  }

  deleteSelf() {
    const route = API_ROUTES.user.delete;
    return this.http.delete(route);
  }
}
