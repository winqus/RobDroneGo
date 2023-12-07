import { APP_INITIALIZER, NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';

import { HttpClientModule } from '@angular/common/http';
import { ReactiveFormsModule } from '@angular/forms';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { EMPTY } from 'rxjs';
import { AppRoutingModule } from './app-routing.module';
import { AppComponent } from './app.component';
import { AboutUsComponent } from './components/about-us/about-us.component';
import { AppBuildingFloorDropdownListComponent } from './components/app-building-floor-dropdown-list/app-building-floor-dropdown-list.component';
import { BuildingListComponent } from './components/building-list/building-list.component';
import { BuildingMinMaxListComponent } from './components/building-min-max-list/building-min-max-list.component';
import { Campus3dComponent } from './components/campus3d/campus3d.component';
import { ChangeRobotStateComponent } from './components/change-robot-state/change-robot-state.component';
import { ComplexityAnalysisComponent } from './components/complexity-analysis/complexity-analysis.component';
import { CreateBuildingComponent } from './components/create-building/create-building.component';
import { CreateElevatorComponent } from './components/create-elevator/create-elevator.component';
import { CreateFloorComponent } from './components/create-floor/create-floor.component';
import { CreatePassageComponent } from './components/create-passage/create-passage.component';
import { CreateRobotTypeComponent } from './components/create-robot-type/create-robot-type.component';
import { CreateRobotComponent } from './components/create-robot/create-robot.component';
import { CreateRoomComponent } from './components/create-room/create-room.component';
import { CreateUserComponent } from './components/create-user/create-user.component';
import { EditBuildingComponent } from './components/edit-building/edit-building.component';
import { EditElevatorComponent } from './components/edit-elevator/edit-elevator.component';
import { EditFloorComponent } from './components/edit-floor/edit-floor.component';
import { EditPassageComponent } from './components/edit-passage/edit-passage.component';
import { EditUserComponent } from './components/edit-user/edit-user.component';
import { ElevatorListComponent } from './components/elevator-list/elevator-list.component';
import { ErrorComponent } from './components/error/error.component';
import { FloorListComponent } from './components/floor-list/floor-list.component';
import { FloorsServedByElevatorListComponent } from './components/floors-served-by-elevator-list/floors-served-by-elevator-list.component';
import { FloorsToDifBuildsComponent } from './components/floors-to-dif-builds/floors-to-dif-builds.component';
import { FooterComponent } from './components/footer/footer.component';
import { FormErrorListComponent } from './components/form-error-list/form-error-list.component';
import { GdprComponent } from './components/gdpr/gdpr.component';
import { LoginComponent } from './components/login/login.component';
import { LogoutComponent } from './components/logout/logout.component';
import { MbcoComponent } from './components/mbco/mbco.component';
import { NavbarComponent } from './components/navbar/navbar.component';
import { PassageListComponent } from './components/passage-list/passage-list.component';
import { PathsBetweenBuildingsComponent } from './components/paths-between-buildings/paths-between-buildings.component';
import { PublicFolderComponent } from './components/public-folder/public-folder.component';
import { RecoveryStrategyComponent } from './components/recovery-strategy/recovery-strategy.component';
import { RecursiveMenuDropdownComponent } from './components/recursive-menu-dropdown/recursive-menu-dropdown.component';
import { RobotListComponent } from './components/robot-list/robot-list.component';
import { SearchRobotComponent } from './components/search-robot/search-robot.component';
import { SuccessFormMessageComponent } from './components/shared/success-form-message/success-form-message.component';
import { SignupComponent } from './components/signup/signup.component';
import { TermsOfUseComponent } from './components/terms-of-use/terms-of-use.component';
import { UploadMapComponent } from './components/upload-map/upload-map.component';
import { JwtService } from './core/authentication/services/jwt.service';
import { UserService } from './core/authentication/services/user.service';
import { jwtInterceptorProvider } from './core/interceptors/jwt.interceptor';
import { loggingInterceptorProvider } from './core/interceptors/logging.interceptor';
import { mockAuthHttpInterceptorProvider } from './core/interceptors/mock-auth-http.interceptor';
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
    RobotListComponent,
    SearchRobotComponent,
    CreateRobotTypeComponent,
    ChangeRobotStateComponent,
    CreateElevatorComponent,
    EditBuildingComponent,
    PassageListComponent,
    AppBuildingFloorDropdownListComponent,
    EditElevatorComponent,
    AboutUsComponent,
    GdprComponent,
    ComplexityAnalysisComponent,
    MbcoComponent,
    RecoveryStrategyComponent,
    PathsBetweenBuildingsComponent,
    FloorsServedByElevatorListComponent,
    PublicFolderComponent,
    UploadMapComponent,
    EditUserComponent,
    TermsOfUseComponent,
    CreateUserComponent,
  ],
  imports: [BrowserModule, BrowserAnimationsModule, AppRoutingModule, ReactiveFormsModule, HttpClientModule],
  providers: [
    initAuthorizationProvider,
    jwtInterceptorProvider,
    loggingInterceptorProvider,
    // mockAuthHttpInterceptorProvider, // comment this line to use the real auth API
  ],
  bootstrap: [AppComponent],
})
export class AppModule {}
