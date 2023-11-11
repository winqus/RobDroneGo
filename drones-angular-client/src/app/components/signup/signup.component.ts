import { Component, Input } from '@angular/core';
import { DomSanitizer, SafeHtml } from '@angular/platform-browser';


export interface SignupProps {
  nameLabel: string;
  emailLabel: string;
  passwordLabel: string;
  confirmPasswordLabel: string;
  gdprLabel: string;
  namePlaceholder: string;
  emailPlaceholder: string;
  passwordPlaceholder: string;
  confirmPasswordPlaceholder: string;
  signupButtonLabel: string;
}

@Component({
  selector: 'app-signup',
  templateUrl: './signup.component.html',
  styleUrls: ['./signup.component.css']
})
export class SignupComponent {
  @Input() props: SignupProps = this.getDefaultProps();

  constructor(private sanitizer: DomSanitizer) { }

  getDefaultProps(): SignupProps {
    return {
      nameLabel: 'No props',
      emailLabel: 'No props',
      passwordLabel: 'No props',
      confirmPasswordLabel: 'No props',
      gdprLabel: 'No props',
      namePlaceholder: 'No props',
      emailPlaceholder: 'No props',
      passwordPlaceholder: 'No props',
      confirmPasswordPlaceholder: 'No props',
      signupButtonLabel: 'No props',
    };
  }

  get sanitizedGdprLabel(): SafeHtml {
    return this.sanitizer.bypassSecurityTrustHtml(this.props.gdprLabel);
  }
}