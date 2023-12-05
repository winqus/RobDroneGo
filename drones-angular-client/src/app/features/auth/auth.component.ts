import { Component, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { LoginProps } from 'src/app/components/login/login.component';
import { SignupProps } from 'src/app/components/signup/signup.component';
import LoginCredentials from 'src/app/core/authentication/models/loginCredentials.model';
import RegisterCredentials from 'src/app/core/authentication/models/registerCredentials.model';
import { UserService } from 'src/app/core/authentication/services/user.service';
import { AuthMap } from 'src/app/core/mappers/auth.mapper';
import { TEXT_TOKENS as content } from '../../../assets/i18n/_textTokens';

export interface AuthProps {
  loginFormButtonLabel: string;
  signupFormButtonLabel: string;
  login: LoginProps;
  signup: SignupProps;
}

@Component({
  selector: 'app-auth',
  templateUrl: './auth.component.html',
  styleUrls: ['./auth.component.css'],
})
export class AuthComponent implements OnInit {
  formType!: 'login' | 'signup';
  loginComponentData = content.components.auth.login;
  signupComponentData = content.components.auth.signup;
  loginFormButtonLabel = content.components.auth.loginFormButtonLabel;
  signupFormButtonLabel = content.components.auth.signupFormButtonLabel;

  errorResponse: any = [];

  constructor(
    private router: Router,
    private activatedRoute: ActivatedRoute,
    private userService: UserService,
  ) {}

  ngOnInit(): void {
    // Listen to the route parameters or URL segments
    this.activatedRoute.firstChild?.url.subscribe((urlSegment) => {
      this.formType = urlSegment[0]?.path === 'signup' ? 'signup' : 'login';
    });
  }

  setFormType(type: 'login' | 'signup') {
    this.formType = type;
    this.router.navigate(['/auth', type]);
  }

  handleLogin(formData: any) {
    const credentials: LoginCredentials = formData;
    this.userService.login(credentials).subscribe({
      next: (authResponse) => {
        // Handle successful login
        this.router.navigate(['/dashboard']);
      },
      error: (error) => {
        // Handle error
        console.error('login error', error);
        this.errorResponse = error;
      },
    });
  }

  handleSignup(formData: any) {
    const credentials: RegisterCredentials = AuthMap.toRegisterCredentials(formData);
    this.userService.register(credentials).subscribe({
      next: (authResponse) => {
        // Handle successful registration
        this.router.navigate(['/dashboard']);
      },
      error: (error) => {
        // Handle error
        console.error('signup error', error);
        this.errorResponse = error;
      },
    });
  }
}
