import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { AbstractControl, FormControl, FormGroup, ValidationErrors, ValidatorFn, Validators } from '@angular/forms';
import { DomSanitizer, SafeHtml } from '@angular/platform-browser';
import { TEXT_TOKENS as content } from '../../../assets/i18n/_textTokens';
import { environment } from '../../../environments/environment';
import RegisterCredentials from '../../core/authentication/models/registerCredentials.model';

export interface SignupProps {
  firstNameLabel: string;
  firstNamePlaceholder: string;
  lastNameLabel: string;
  lastNamePlaceholder: string;
  emailLabel: string;
  emailPlaceholder: string;
  phonenumberLabel: string;
  phonenumberPlaceholder: string;
  taxpayernumberLabel: string;
  taxpayernumberPlaceholder: string;
  passwordLabel: string;
  passwordPlaceholder: string;
  confirmPasswordLabel: string;
  confirmPasswordPlaceholder: string;
  gdprLabel: string;
  signupButtonLabel: string;
}

@Component({
  selector: 'app-signup',
  templateUrl: './signup.component.html',
  styleUrls: ['./signup.component.css'],
})
export class SignupComponent implements OnInit {
  @Input() props: SignupProps = this.getDefaultProps();

  @Output() submitEvent = new EventEmitter<any>();

  signupForm!: FormGroup;
  validationErrors = content.validation_errors;
  firstNameArgs = { field: 'First Name', min: 2, max: 50 };
  lastNameArgs = { field: 'Last Name', min: 2, max: 50 };
  passwordArgs = { field: 'Password', min: 10, max: 50 };
  taxpayernumberArgs = { field: 'Taxpayernumber', min: 9, max: 9 };
  emailDomainName = environment.emailDomain;

  constructor(private sanitizer: DomSanitizer) {}

  ngOnInit(): void {
    this.signupForm = new FormGroup(
      {
        firstName: new FormControl('', [Validators.required, Validators.minLength(this.firstNameArgs.min), Validators.maxLength(this.firstNameArgs.max)]),
        lastName: new FormControl('', [Validators.required, Validators.minLength(this.lastNameArgs.min), Validators.maxLength(this.lastNameArgs.max)]),
        email: new FormControl('', [Validators.required, Validators.email, this.emailDomainValidator()]),
        phonenumber: new FormControl('', [Validators.required, Validators.pattern('^[0-9]{9}$')]),
        taxpayernumber: new FormControl('', [Validators.required, Validators.pattern('^[0-9]{9}$')]),
        password: new FormControl('', [Validators.required, Validators.minLength(10), Validators.pattern(/^(?=.*[A-Z])(?=.*[a-z])(?=.*\d)(?=.*[@$!%*?&.,#^+])[A-Za-z\d@$!%*?&.,#^+]{10,}$/)]),
        confirmPassword: new FormControl('', Validators.required),
        gdprCompliance: new FormControl(false, Validators.requiredTrue),
        isConfirmed: new FormControl(false),
      },
      { validators: this.passwordMatchValidator },
    );
  }

  getDefaultProps(): SignupProps {
    return {
      firstNameLabel: 'No props',
      lastNameLabel: 'No props',
      emailLabel: 'No props',
      passwordLabel: 'No props',
      confirmPasswordLabel: 'No props',
      gdprLabel: 'No props',
      firstNamePlaceholder: 'No props',
      lastNamePlaceholder: 'No props',
      emailPlaceholder: 'No props',
      phonenumberLabel: 'No props',
      phonenumberPlaceholder: 'No props',
      taxpayernumberLabel: 'No props',
      taxpayernumberPlaceholder: 'No props',
      passwordPlaceholder: 'No props',
      confirmPasswordPlaceholder: 'No props',
      signupButtonLabel: 'No props',
    };
  }

  emailDomainValidator(): ValidatorFn {
    return (control: AbstractControl): ValidationErrors | null => {
      const email = control.value as string;

      if (email && !email.endsWith(`@${environment.emailDomain}`)) {
        return { invalidDomain: true };
      }

      return null;
    };
  }
  passwordMatchValidator: ValidatorFn = (control: AbstractControl): ValidationErrors | null => {
    const password = control.get('password');
    const confirmPassword = control.get('confirmPassword');

    return password && confirmPassword && password.value === confirmPassword.value ? null : { mismatch: true };
  };

  get sanitizedGdprLabel(): SafeHtml {
    return this.sanitizer.bypassSecurityTrustHtml(this.props.gdprLabel);
  }

  onSubmit() {
    this.submitEvent.emit(this.signupForm.value as RegisterCredentials);
  }
}
