import { APP_INITIALIZER, NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';

import { HttpClientModule } from '@angular/common/http';
import { ReactiveFormsModule } from '@angular/forms';
import { EMPTY } from 'rxjs';
import { AppRoutingModule } from './app-routing.module';
import { AppComponent } from './app.component';
import { Campus3dComponent } from './components/campus3d/campus3d.component';
import { ErrorComponent } from './components/error/error.component';
import { FooterComponent } from './components/footer/footer.component';
import { LoginComponent } from './components/login/login.component';
import { LogoutComponent } from './components/logout/logout.component';
import { NavbarComponent } from './components/navbar/navbar.component';
import { SignupComponent } from './components/signup/signup.component';
import { JwtService } from './core/authentication/services/jwt.service';
import { UserService } from './core/authentication/services/user.service';
import { jwtInterceptorProvider } from './core/interceptors/jwt.interceptor';
import { mockHttpInterceptorProvider } from './core/interceptors/mock-http.interceptor';
import { EmptyLayoutComponent } from './core/layouts/empty-layout/empty-layout.component';
import { MainLayoutComponent } from './core/layouts/main-layout/main-layout.component';
import { AuthComponent } from './features/auth/auth.component';
import { DashboardComponent } from './features/dashboard/dashboard.component';
import { ErrorMessagePipe } from './shared/pipes/error-message.pipe';

export function initAuthorization(jwtService: JwtService, userService: UserService) {
  return () => (jwtService.getToken() ? userService.getCurrentUser() : EMPTY);
}

export const initAuthorizationProvider = {
  provide: APP_INITIALIZER,
  useFactory: initAuthorization,
  deps: [JwtService, UserService],
  multi: true,
}; 

@NgModule({
  declarations: [
    AppComponent,
    MainLayoutComponent,
    EmptyLayoutComponent,
    NavbarComponent,
    DashboardComponent,
    FooterComponent,
    ErrorComponent,
    LoginComponent,
    SignupComponent,
    AuthComponent,
    Campus3dComponent,
    LogoutComponent,
    ErrorMessagePipe,
  ],
  imports: [
    BrowserModule,
    AppRoutingModule,
    ReactiveFormsModule,
    HttpClientModule,
  ],
  providers: [
    initAuthorizationProvider,
    jwtInterceptorProvider,
    // comment this line to use the real API
    mockHttpInterceptorProvider, 
  ],
  bootstrap: [AppComponent]
})
export class AppModule { }
