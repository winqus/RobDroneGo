import { NgModule } from '@angular/core';
import { ReactiveFormsModule } from '@angular/forms';
import { RouterModule, Routes } from '@angular/router';
import { TEXT_TOKENS as content } from '../assets/i18n/_textTokens';
import { AboutUsComponent } from './components/about-us/about-us.component';
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
import { CreateTaskRequestComponent } from './components/create-task-request/create-task-request.component';
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
import { GdprComponent } from './components/gdpr/gdpr.component';
import { ListTaskRequestComponent } from './components/list-task-request/list-task-request.component';
import { LoginComponent } from './components/login/login.component';
import { LogoutComponent } from './components/logout/logout.component';
import { MbcoComponent } from './components/mbco/mbco.component';
import { PassageListComponent } from './components/passage-list/passage-list.component';
import { PathsBetweenBuildingsComponent } from './components/paths-between-buildings/paths-between-buildings.component';
import { PublicFolderComponent } from './components/public-folder/public-folder.component';
import { RecoveryStrategyComponent } from './components/recovery-strategy/recovery-strategy.component';
import { RobotListComponent } from './components/robot-list/robot-list.component';
import { SearchRobotComponent } from './components/search-robot/search-robot.component';
import { SearchTaskComponent } from './components/search-task/search-task.component';
import { SignupComponent } from './components/signup/signup.component';
import { UploadMapComponent } from './components/upload-map/upload-map.component';
import { UserListComponent } from './components/user-list/user-list.component';
import { canActivateChildWithAuth, canActivateChildWithRole, canActivateWithAuth, canActivateWithRole } from './core/authentication/guards/auth.guard';
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
      { path: 'dashboard', component: DashboardComponent, data: { title: 'Dashboard' } },
      { path: '3d', component: Campus3dComponent, data: { title: '3D' }, canActivate: [canActivateWithRole([UserRole.FleetManager, UserRole.TaskManager, UserRole.CampusManager])] },
      {
        path: '3d/building/:buildingCode/floor/:floorNumber',
        component: Campus3dComponent,
        data: { title: '3D' },
        canActivate: [canActivateWithRole([UserRole.FleetManager, UserRole.TaskManager, UserRole.CampusManager])],
      },
      {
        path: 'campus',
        data: { title: 'Campus' },
        children: [
          { path: '', redirectTo: '/dashboard', pathMatch: 'full' },
          {
            path: 'building',
            data: { title: 'Building' },
            canActivate: [canActivateChildWithRole([UserRole.CampusManager])],
            children: [
              { path: '', redirectTo: 'list', pathMatch: 'full' },
              { path: ':code/floors', component: FloorListComponent, data: { title: 'Floors' } },
              { path: ':code/floors/:number/edit', component: EditFloorComponent, data: { title: 'Edit Floor' } },
              { path: 'create', component: CreateBuildingComponent, data: { title: 'Create Building' } },
              { path: 'list', component: BuildingListComponent, data: { title: 'Buildings' } },
              { path: ':id/edit', component: EditBuildingComponent, data: { title: 'Edit Building' } },
              { path: ':code/elevators', component: ElevatorListComponent, data: { title: 'Elevators' } },
              { path: ':code/elevator/:number/edit', component: EditElevatorComponent, data: { title: 'Edit Elevator' } },
              { path: ':minFloor/:maxFloor', component: BuildingMinMaxListComponent, data: { title: 'Buildings' } },
              { path: ':code/floorsWithPassages', component: FloorsToDifBuildsComponent, data: { title: 'Floors With Passages' } },
            ],
          },
          {
            path: 'floor',
            data: { title: 'Floor' },
            canActivate: [canActivateChildWithRole([UserRole.CampusManager])],
            children: [
              { path: '', redirectTo: '/dashboard', pathMatch: 'full' },
              { path: 'create', component: CreateFloorComponent, data: { title: 'Create Floor' } },
              { path: 'served-by-elevator', component: FloorsServedByElevatorListComponent, data: { title: 'Floors Served By Elevator' } },
            ],
          },
          {
            path: 'passage',
            data: { title: 'Passage' },
            canActivate: [canActivateChildWithRole([UserRole.CampusManager])],
            children: [
              { path: '', redirectTo: '/dashboard', pathMatch: 'full' },
              { path: 'create', component: CreatePassageComponent, data: { title: 'Create Passage' } },
              { path: 'edit', component: EditPassageComponent, data: { title: 'Edit Passage' } },
              { path: 'list', component: PassageListComponent, data: { title: 'Passages' } },
            ],
          },
          {
            path: 'room',
            data: { title: 'Room' },
            canActivate: [canActivateChildWithRole([UserRole.CampusManager])],
            children: [
              { path: '', redirectTo: '/dashboard', pathMatch: 'full' },
              { path: 'create', component: CreateRoomComponent, data: { title: 'Create Room' } },
            ],
          },
          {
            path: 'elevator',
            data: { title: 'Elevator' },
            canActivate: [canActivateChildWithRole([UserRole.CampusManager])],
            children: [
              { path: '', redirectTo: '/dashboard', pathMatch: 'full' },
              { path: 'create', component: CreateElevatorComponent, data: { title: 'Create Elevator' } },
              { path: 'edit', component: EditElevatorComponent, data: { title: 'Edit Elevator' } },
            ],
          },
          {
            path: 'map',
            data: { title: 'Map' },
            canActivate: [canActivateWithRole([UserRole.CampusManager])],
            children: [{ path: 'upload', component: UploadMapComponent, data: { title: 'Upload Map' } }],
          },
          // ... other campus-related routes here
        ],
      },
      {
        path: 'fleet',
        data: { title: 'Fleet' },
        canActivate: [canActivateChildWithRole([UserRole.FleetManager])],
        children: [
          { path: '', redirectTo: '/dashboard', pathMatch: 'full' },
          {
            path: 'robot',
            data: { title: 'Robot' },
            children: [
              { path: '', redirectTo: '/dashboard', pathMatch: 'full' },
              { path: 'create', component: CreateRobotComponent, data: { title: 'Create Robot' } },
              { path: 'list', component: RobotListComponent, data: { title: 'Robots' } },
              { path: 'state', component: ChangeRobotStateComponent, data: { title: 'Change Robot State' } },
              { path: 'search', component: SearchRobotComponent, data: { title: 'Search Robot' } },
            ],
          },
          {
            path: 'robotType',
            data: { title: 'Robot Type' },
            children: [
              { path: '', redirectTo: '/dashboard', pathMatch: 'full' },
              { path: 'create', component: CreateRobotTypeComponent, data: { title: 'Create Robot Type' } },
            ],
          },
        ],
      },
      {
        path: 'task',
        data: { title: 'Task' },
        canActivate: [canActivateChildWithRole([UserRole.User, UserRole.TaskManager])],
        children: [
          { path: '', redirectTo: '/dashboard', pathMatch: 'full' },
          { path: 'request-task', component: CreateTaskRequestComponent, data: { title: 'Request Task' } },
          { path: 'search', component: SearchTaskComponent, data: { title: 'Search Task' } },
          { path: 'route', canActivate: [canActivateWithRole([UserRole.TaskManager])], component: PathsBetweenBuildingsComponent, data: { title: 'Route' } },
          { path: 'analysis', canActivate: [canActivateWithRole([UserRole.TaskManager])], component: ComplexityAnalysisComponent, data: { title: 'Analysis' } },
          { path: 'list', canActivate: [canActivateWithRole([UserRole.TaskManager])], component: ListTaskRequestComponent, data: { title: 'List' } },
        ],
      },
      {
        path: 'system',
        data: { title: 'System' },
        canActivate: [canActivateChildWithRole([UserRole.SystemAdministrator])],
        children: [
          { path: '', redirectTo: '/dashboard', pathMatch: 'full' },
          { path: 'mbco', component: MbcoComponent, data: { title: 'MBCO' } },
          { path: 'recovery-strategy', component: RecoveryStrategyComponent, data: { title: 'Recovery Strategy' } },
          { path: 'create-user', component: CreateUserComponent, data: { title: 'Create User' } },
          { path: 'users', component: UserListComponent, data: { title: 'Users' } },
        ],
      },
      {
        path: 'about',
        data: { title: 'About' },
        children: [
          { path: '', redirectTo: '/dashboard', pathMatch: 'full' },
          {
            path: 'info',
            component: AboutUsComponent,
            data: { title: 'About Us' },
          },

          {
            path: 'privacy-policy',
            component: GdprComponent,
            data: { title: 'Privacy Policy' },
          },

          {
            path: 'folder',
            data: { title: 'Public Folder' },
            canActivate: [canActivateWithRole([UserRole.User, UserRole.CampusManager, UserRole.FleetManager, UserRole.SystemAdministrator, UserRole.TaskManager])],
            children: [{ path: '', component: PublicFolderComponent }],
          },
        ],
      },

      // ... other routes here (that need the main layout)
    ],
  },
  {
    path: 'about/public',
    data: { title: 'Public' },
    component: EmptyLayoutComponent,
    children: [
      { path: '', redirectTo: '/dashboard', pathMatch: 'full' },
      {
        path: 'privacy-policy',
        component: GdprComponent,
        data: { title: 'Privacy Policy' },
      },
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
          { path: 'signup', component: SignupComponent, data: { title: 'Sign Up' } },
          { path: 'login', component: LoginComponent, data: { title: 'Log In' } },

          { path: '', redirectTo: 'signup', pathMatch: 'full' },
        ],
      },
      {
        path: '',
        component: MainLayoutComponent,
        children: [{ path: 'user', component: EditUserComponent, data: { title: 'Profile' } }],
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
          title: '403 Forbidden',
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
          title: '404 Not Found',
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
