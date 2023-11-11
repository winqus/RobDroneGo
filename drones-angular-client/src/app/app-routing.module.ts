import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { TEXT_TOKENS as content } from '../assets/i18n/_textTokens';
import { Campus3dComponent } from './components/campus3d/campus3d.component';
import { ErrorComponent } from './components/error/error.component';
import { LoginComponent } from './components/login/login.component';
import { SignupComponent } from './components/signup/signup.component';
import { EmptyLayoutComponent } from './core/layouts/empty-layout/empty-layout.component';
import { MainLayoutComponent } from './core/layouts/main-layout/main-layout.component';
import { AuthComponent } from './features/auth/auth.component';
import { DashboardComponent } from './features/dashboard/dashboard.component';

const routes: Routes = [
  {
    path: '',
    component: MainLayoutComponent,
    children: [
      { path: 'dashboard', component: DashboardComponent },
      { path: '3d', component: Campus3dComponent },
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
          { path: '', redirectTo: 'signup', pathMatch: 'full' }
        ]
      }
    ]
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
          redirectAfter: 3000,
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
          redirectAfter: 3000,
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
export class AppRoutingModule { }
