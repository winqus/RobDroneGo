import { Component, EventEmitter, Input, Output } from '@angular/core';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { TEXT_TOKENS as content } from '../../../assets/i18n/_textTokens';

export interface LoginProps {
  emailLabel: string;
  passwordLabel: string;
  emailPlaceholder: string;
  passwordPlaceholder: string;
  loginButtonLabel: string;
}

export interface User {
  email: string;
  password: string;
  isConfirmed: boolean;
}

@Component({
  selector: 'app-login',
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.css'],
})
export class LoginComponent {
  @Input() props: LoginProps = this.getDefaultProps();

  @Output() submitEvent = new EventEmitter<User>();

  loginForm: FormGroup;
  validationErrors = content.validation_errors;
  user: any;

  constructor() {
    this.loginForm = new FormGroup({
      email: new FormControl('', [Validators.required, Validators.email]),
      password: new FormControl('', [Validators.required, Validators.minLength(8)]),
    });
  }

  getDefaultProps(): LoginProps {
    return {
      emailLabel: 'No props',
      passwordLabel: 'No props',
      emailPlaceholder: 'No props',
      passwordPlaceholder: 'No props',
      loginButtonLabel: 'No props',
    };
  }

  onSubmit() {
    const user: User = this.loginForm.value;

    if (user.isConfirmed) {
      this.submitEvent.emit(user);
    } else {
      console.log('User is not confirmed. Login not allowed.');
    }
  }
}
