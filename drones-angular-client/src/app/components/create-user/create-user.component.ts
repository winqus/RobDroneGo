import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { AbstractControl, FormControl, FormGroup, ValidationErrors, ValidatorFn, Validators } from '@angular/forms';
import { DomSanitizer, SafeHtml } from '@angular/platform-browser';
import { Router } from '@angular/router';
import RegisterCredentials from 'src/app/core/authentication/models/registerCredentials.model';
import { UserRole } from 'src/app/core/authentication/models/user-roles.enum';
import { SystemAdminService } from 'src/app/services/system-admin.service';
import { environment } from 'src/environments/environment';
import { TEXT_TOKENS as content } from '../../../assets/i18n/_textTokens';
import { SuccessMessage } from '../shared/success-form-message/success-form-message.component';

export interface CreateUserProps {
  firstNameLabel: string;
  firstNamePlaceholder: string;
  lastNameLabel: string;
  lastNamePlaceholder: string;
  emailLabel: string;
  emailPlaceholder: string;
  phonenumberLabel: string;
  phonenumberPlaceholder: string;
  passwordLabel: string;
  passwordPlaceholder: string;
  confirmPasswordLabel: string;
  confirmPasswordPlaceholder: string;
  userRolesDropdownLabel: string;
  userRoles: { label: string; role: string }[];
  createUserButtonLabel: string;
  userCreatedMessage: string;
}

export interface CreateUserCredentials {
  firstName: string;
  lastName: string;
  email: string;
  phonenumber: string;
  taxpayernumber: string;
  password: string;
  role: string;
}

@Component({
  selector: 'app-create-user',
  templateUrl: './create-user.component.html',
  styleUrls: ['./create-user.component.css'],
})
export class CreateUserComponent implements OnInit {
  @Input() props: CreateUserProps = content.components.createUser || this.getDefaultProps();
  @Output() submitEvent = new EventEmitter<any>();

  createUserForm!: FormGroup;
  validationErrors = content.validation_errors;
  firstNameArgs = { field: 'First Name', min: 2, max: 50 };
  lastNameArgs = { field: 'Last Name', min: 2, max: 50 };
  passwordArgs = { field: 'Password', min: 10, max: 50 };
  emailDomainName = environment.emailDomain;

  errorResponse: any = [];
  submitSuccessMessage: SuccessMessage = null;

  constructor(
    private sanitizer: DomSanitizer,
    private router: Router,
    private systemAdminService: SystemAdminService,
  ) {}

  ngOnInit(): void {
    this.createUserForm = new FormGroup(
      {
        firstName: new FormControl('', [Validators.required, Validators.minLength(this.firstNameArgs.min), Validators.maxLength(this.firstNameArgs.max)]),
        lastName: new FormControl('', [Validators.required, Validators.minLength(this.lastNameArgs.min), Validators.maxLength(this.lastNameArgs.max)]),
        email: new FormControl('', [Validators.required, Validators.email, this.emailDomainValidator()]),
        phonenumber: new FormControl('', [Validators.required, Validators.pattern(/^[\d+]+$/)]),
        password: new FormControl('', [Validators.required, Validators.minLength(10), Validators.pattern(/^(?=.*[A-Z])(?=.*[a-z])(?=.*\d)(?=.*[@$!%*?&.,#^+])[A-Za-z\d@$!%*?&.,#^+]{10,}$/)]),
        confirmPassword: new FormControl('', Validators.required),
        role: new FormControl(this.props.userRoles[0].role, Validators.required),
      },
      { validators: this.passwordMatchValidator },
    );
  }

  getDefaultProps(): CreateUserProps {
    return {
      firstNameLabel: 'No props',
      lastNameLabel: 'No props',
      emailLabel: 'No props',
      passwordLabel: 'No props',
      confirmPasswordLabel: 'No props',
      firstNamePlaceholder: 'No props',
      lastNamePlaceholder: 'No props',
      emailPlaceholder: 'No props',
      phonenumberLabel: 'No props',
      phonenumberPlaceholder: 'No props',
      passwordPlaceholder: 'No props',
      confirmPasswordPlaceholder: 'No props',
      userRolesDropdownLabel: 'No props',
      userRoles: [],
      createUserButtonLabel: 'No props',
      userCreatedMessage: 'No props',
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

  onSubmit() {
    // this.submitEvent.emit(this.createUserForm.value as RegisterCredentials);
    const credentials: CreateUserCredentials = {
      ...this.createUserForm.value,
      taxpayernumber: '000000000',
    };
    this.systemAdminService.createUser(credentials).subscribe({
      next: (authResponse) => {
        this.submitSuccessMessage = this.props.userCreatedMessage;
      },
      error: (error) => {
        // Handle error
        console.error('login error', error);
        this.errorResponse = error;
      },
    });
  }
}
