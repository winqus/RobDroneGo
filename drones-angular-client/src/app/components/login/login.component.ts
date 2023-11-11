import { Component, Input } from '@angular/core';

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

  getDefaultProps(): LoginProps {
    return {
      emailLabel: 'No props',
      passwordLabel: 'No props',
      emailPlaceholder: 'No props',
      passwordPlaceholder: 'No props',
      loginButtonLabel: 'No props',
    };
  }
}
