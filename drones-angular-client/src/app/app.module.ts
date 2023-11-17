import { APP_INITIALIZER, NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';

import { HttpClientModule } from '@angular/common/http';
import { ReactiveFormsModule } from '@angular/forms';
import { EMPTY } from 'rxjs';
import { AppRoutingModule } from './app-routing.module';
import { AppComponent } from './app.component';
import { BuildingListComponent } from './components/building-list/building-list.component';
import { BuildingMinMaxListComponent } from './components/building-min-max-list/building-min-max-list.component';
import { Campus3dComponent } from './components/campus3d/campus3d.component';
import { CreateBuildingComponent } from './components/create-building/create-building.component';
import { CreateFloorComponent } from './components/create-floor/create-floor.component';
import { CreatePassageComponent } from './components/create-passage/create-passage.component';
import { CreateRobotComponent } from './components/create-robot/create-robot.component';
import { CreateRoomComponent } from './components/create-room/create-room.component';
import { EditFloorComponent } from './components/edit-floor/edit-floor.component';
import { EditPassageComponent } from './components/edit-passage/edit-passage.component';
import { ElevatorListComponent } from './components/elevator-list/elevator-list.component';
import { ErrorComponent } from './components/error/error.component';
import { FloorListComponent } from './components/floor-list/floor-list.component';
import { FloorsToDifBuildsComponent } from './components/floors-to-dif-builds/floors-to-dif-builds.component';
import { FooterComponent } from './components/footer/footer.component';
import { FormErrorListComponent } from './components/form-error-list/form-error-list.component';
import { LoginComponent } from './components/login/login.component';
import { LogoutComponent } from './components/logout/logout.component';
import { NavbarComponent } from './components/navbar/navbar.component';
import { RecursiveMenuDropdownComponent } from './components/recursive-menu-dropdown/recursive-menu-dropdown.component';
import { SuccessFormMessageComponent } from './components/shared/success-form-message/success-form-message.component';
import { SignupComponent } from './components/signup/signup.component';
import { JwtService } from './core/authentication/services/jwt.service';
import { UserService } from './core/authentication/services/user.service';
import { jwtInterceptorProvider } from './core/interceptors/jwt.interceptor';
import { loggingInterceptorProvider } from './core/interceptors/logging.interceptor';
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
    BuildingListComponent,
    CreatePassageComponent,
    FormErrorListComponent,
    FloorListComponent,
    CreateBuildingComponent,
    SuccessFormMessageComponent,
    RecursiveMenuDropdownComponent,
    FloorListComponent,
    EditFloorComponent,
    ElevatorListComponent,
    CreateFloorComponent,
    FloorsToDifBuildsComponent,
    BuildingMinMaxListComponent,
    CreateRobotComponent,
    EditPassageComponent,
    CreateRoomComponent,
  ],
  imports: [BrowserModule, AppRoutingModule, ReactiveFormsModule, HttpClientModule],
  providers: [
    initAuthorizationProvider,
    jwtInterceptorProvider,
    loggingInterceptorProvider,
    // comment this line to use the real API
    mockHttpInterceptorProvider,
  ],
  bootstrap: [AppComponent],
})
export class AppModule {}
