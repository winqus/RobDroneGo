import { inject } from '@angular/core';
import {
  ActivatedRouteSnapshot,
  CanActivateChildFn,
  CanActivateFn,
  Router,
} from '@angular/router';
import { Observable } from 'rxjs';
import { map } from 'rxjs/internal/operators/map';
import { take } from 'rxjs/internal/operators/take';
import { UserRole } from '../models/user-roles.enum';
import { UserService } from '../services/user.service';

export const canActivateWithAuth: CanActivateFn = (): Observable<boolean> | boolean => {
  const userService = inject(UserService);
  const router = inject(Router);

  return userService.isAuthenticated.pipe(
    take(1), // Take only the first emitted value and then complete
    map((isAuth) => {
      if (isAuth) {
        return true;
      } else {
        router.navigate(['/auth/signup']);
        return false;
      }
    })
  );
};

export const canActivateWithRole: (
  allowedRoles: UserRole[]
) => CanActivateFn = (allowedRoles) => (route: ActivatedRouteSnapshot) => {
  const userService = inject(UserService);

  if (userService.hasRole(allowedRoles)) {
    return true;
  } else {
    inject(Router).navigate(['/error/403']);
    return false;
  }
};

export const canActivateChildWithAuth: CanActivateChildFn = canActivateWithAuth;

export const canActivateChildWithRole: (allowedRoles: UserRole[]) => CanActivateChildFn = (allowedRoles) => {
  return (route, state) => canActivateWithRole(allowedRoles)(route, state);
};