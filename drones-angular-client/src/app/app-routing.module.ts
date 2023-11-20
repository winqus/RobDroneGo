import { NgModule } from '@angular/core';
import { ReactiveFormsModule } from '@angular/forms';
import { RouterModule, Routes } from '@angular/router';
import { TEXT_TOKENS as content } from '../assets/i18n/_textTokens';
import { BuildingListComponent } from './components/building-list/building-list.component';
import { BuildingMinMaxListComponent } from './components/building-min-max-list/building-min-max-list.component';
import { Campus3dComponent } from './components/campus3d/campus3d.component';
import { ChangeRobotStateComponent } from './components/change-robot-state/change-robot-state.component';
import { CreateBuildingComponent } from './components/create-building/create-building.component';
import { CreateElevatorComponent } from './components/create-elevator/create-elevator.component';
import { CreateFloorComponent } from './components/create-floor/create-floor.component';
import { CreatePassageComponent } from './components/create-passage/create-passage.component';
import { CreateRobotTypeComponent } from './components/create-robot-type/create-robot-type.component';
import { CreateRobotComponent } from './components/create-robot/create-robot.component';
import { CreateRoomComponent } from './components/create-room/create-room.component';
import { EditBuildingComponent } from './components/edit-building/edit-building.component';
import { EditFloorComponent } from './components/edit-floor/edit-floor.component';
import { EditPassageComponent } from './components/edit-passage/edit-passage.component';
import { ElevatorListComponent } from './components/elevator-list/elevator-list.component';
import { ErrorComponent } from './components/error/error.component';
import { FloorListComponent } from './components/floor-list/floor-list.component';
import { FloorsToDifBuildsComponent } from './components/floors-to-dif-builds/floors-to-dif-builds.component';
import { LoginComponent } from './components/login/login.component';
import { LogoutComponent } from './components/logout/logout.component';
import { PassageListComponent } from './components/passage-list/passage-list.component';
import { RobotListComponent } from './components/robot-list/robot-list.component';
import { SearchRobotComponent } from './components/search-robot/search-robot.component';
import { SignupComponent } from './components/signup/signup.component';
import { canActivateChildWithAuth, canActivateWithAuth, canActivateWithRole } from './core/authentication/guards/auth.guard';
import { UserRole } from './core/authentication/models/user-roles.enum';
import { EmptyLayoutComponent } from './core/layouts/empty-layout/empty-layout.component';
import { MainLayoutComponent } from './core/layouts/main-layout/main-layout.component';
import { AuthComponent } from './features/auth/auth.component';
import { DashboardComponent } from './features/dashboard/dashboard.component';

const routes: Routes = [
  {
    path: '',
    redirectTo: '/dashboard',
    pathMatch: 'full',
  },
  {
    path: '',
    component: MainLayoutComponent,
    canActivateChild: [canActivateChildWithAuth],
    children: [
      { path: 'dashboard', component: DashboardComponent },
      { path: '3d', component: Campus3dComponent, canActivate: [canActivateWithRole([UserRole.User, UserRole.CampusManager])] },
      { path: '3d/building/:buildingCode/floor/:floorNumber', component: Campus3dComponent, canActivate: [canActivateWithRole([UserRole.User, UserRole.CampusManager])] },
      {
        path: 'campus',
        children: [
          { path: '', redirectTo: '/dashboard', pathMatch: 'full' },
          {
            path: 'building',
            canActivate: [canActivateWithRole([UserRole.User, UserRole.CampusManager])],
            children: [
              { path: '', redirectTo: 'list', pathMatch: 'full' },
              { path: ':code/floors', component: FloorListComponent, canActivate: [canActivateWithRole([UserRole.User, UserRole.CampusManager])] },
              { path: ':code/floors/:number/edit', component: EditFloorComponent, canActivate: [canActivateWithRole([UserRole.User, UserRole.CampusManager])] },
              { path: 'create', component: CreateBuildingComponent, canActivate: [canActivateWithRole([UserRole.User])] },
              { path: 'list', component: BuildingListComponent, canActivate: [canActivateWithRole([UserRole.User])] },
              { path: ':id/edit', component: EditBuildingComponent, canActivate: [canActivateWithRole([UserRole.User, UserRole.CampusManager])] },
              { path: ':code/elevators', component: ElevatorListComponent, canActivate: [canActivateWithRole([UserRole.User])] },
              { path: ':minFloor/:maxFloor', component: BuildingMinMaxListComponent, canActivate: [canActivateWithRole([UserRole.User])] },
              { path: ':code/floorsWithPassages', component: FloorsToDifBuildsComponent },
            ],
          },
          {
            path: 'floor',
            //canActivate: [canActivateWithRole([UserRole.User, UserRole.CampusManager])],
            children: [{ path: 'create', component: CreateFloorComponent }],
          },
          {
            path: 'passage',
            canActivate: [canActivateWithRole([UserRole.User, UserRole.CampusManager])],
            children: [
              { path: '', redirectTo: 'create', pathMatch: 'full' },
              { path: 'create', component: CreatePassageComponent },
              { path: 'edit', component: EditPassageComponent },
              { path: 'list', component: PassageListComponent },
            ],
          },
          {
            path: 'room',
            canActivate: [canActivateWithRole([UserRole.User, UserRole.CampusManager])],
            children: [{ path: 'create', component: CreateRoomComponent }],
          },
          {
            path: 'elevator',
            canActivate: [canActivateWithRole([UserRole.User, UserRole.CampusManager])],
            children: [{ path: 'create', component: CreateElevatorComponent }],
          },
          // ... other campus-related routes here
        ],
      },
      {
        path: 'fleet',
        children: [
          { path: '', redirectTo: '/dashboard', pathMatch: 'full' },
          {
            path: 'robot',
            canActivate: [canActivateWithRole([UserRole.User, UserRole.FleetManager])],
            children: [
              { path: 'create', component: CreateRobotComponent },
              { path: 'list', component: RobotListComponent },
              { path: 'state', component: ChangeRobotStateComponent },
              { path: ':?', component: SearchRobotComponent },
            ],
          },
          {
            path: 'robotType',
            canActivate: [canActivateWithRole([UserRole.User, UserRole.FleetManager])],
            children: [{ path: 'create', component: CreateRobotTypeComponent }],
          },
        ],
      },
      // ... other routes here (that need the main layout)
    ],
  },
  {
    path: 'auth',
    component: EmptyLayoutComponent,
    children: [
      {
        path: '',
        component: AuthComponent,
        children: [
          { path: 'signup', component: SignupComponent },
          { path: 'login', component: LoginComponent },
          { path: '', redirectTo: 'signup', pathMatch: 'full' },
        ],
      },
      { path: 'logout', component: LogoutComponent },
    ],
  },
  {
    path: 'error',
    component: EmptyLayoutComponent,
    children: [
      {
        path: '403',
        component: ErrorComponent,
        data: {
          errorCode: content.http.error403Forbidden.status,
          errorMessage: content.http.error403Forbidden.shortMessage,
          infoMessage: content.components.error.redirectingText,
          redirectAfter: 1000,
          redirectTo: '/dashboard',
        },
      },
      {
        path: '404',
        component: ErrorComponent,
        data: {
          errorCode: content.http.error404NotFound.status,
          errorMessage: content.http.error404NotFound.shortMessage,
          infoMessage: content.components.error.redirectingText,
          redirectAfter: 2000,
          redirectTo: '/dashboard',
        },
      },
    ],
  },
  // ... other routes
  { path: '**', redirectTo: '/error/404' }, // always last for handling invalid routes
];
@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule],
})
export class AppRoutingModule {}
