import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { AbstractControl, FormControl, FormGroup, ValidationErrors, ValidatorFn, Validators } from '@angular/forms';
import { DomSanitizer, SafeHtml } from '@angular/platform-browser';
import RegisterCredentials from 'src/app/core/authentication/models/registerCredentials.model';
import { UserRole } from 'src/app/core/authentication/models/user-roles.enum';
import { TEXT_TOKENS as content } from '../../../assets/i18n/_textTokens';

export interface SignupProps {
  firstNameLabel: string;
  firstNamePlaceholder: string;
  lastNameLabel: string;
  lastNamePlaceholder: string;
  emailLabel: string;
  emailPlaceholder: string;
  passwordLabel: string;
  passwordPlaceholder: string;
  confirmPasswordLabel: string;
  confirmPasswordPlaceholder: string;
  userRolesDropdownLabel: string;
  userRoles: { label: string; role: string }[];
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
  passwordArgs = { field: 'Password', min: 8, max: 50 };

  constructor(private sanitizer: DomSanitizer) {}

  ngOnInit(): void {
    // Listen to the route parameters or URL segments
    this.signupForm = new FormGroup(
      {
        firstName: new FormControl('', [Validators.required, Validators.minLength(this.firstNameArgs.min), Validators.maxLength(this.firstNameArgs.max)]),
        lastName: new FormControl('', [Validators.required, Validators.minLength(this.lastNameArgs.min), Validators.maxLength(this.lastNameArgs.max)]),
        email: new FormControl('', [Validators.required, Validators.email]),
        password: new FormControl('', [Validators.required, Validators.minLength(this.passwordArgs.min), Validators.maxLength(this.passwordArgs.max)]),
        confirmPassword: new FormControl('', Validators.required),
        role: new FormControl(this.props.userRoles[0].role, Validators.required),
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
      passwordPlaceholder: 'No props',
      confirmPasswordPlaceholder: 'No props',
      userRolesDropdownLabel: 'No props',
      userRoles: [],
      signupButtonLabel: 'No props',
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
