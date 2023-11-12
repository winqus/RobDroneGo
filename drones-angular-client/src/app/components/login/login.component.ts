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

@Component({
  selector: 'app-login',
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.css']
})
export class LoginComponent {
  @Input() props: LoginProps = this.getDefaultProps();

  @Output() submitEvent = new EventEmitter<any>();

  loginForm: FormGroup;
  validationErrors = content.validation_errors;

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
    this.submitEvent.emit(this.loginForm.value);
  }
}
