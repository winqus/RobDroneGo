import { Component, EventEmitter, Input, OnChanges, OnInit, Output } from '@angular/core';
import { AbstractControl, FormControl, FormGroup, ValidationErrors, ValidatorFn, Validators } from '@angular/forms';
import { ActivatedRoute } from '@angular/router';
import { UserRole } from 'src/app/core/authentication/models/user-roles.enum';
import { User } from 'src/app/core/authentication/models/user.model';
import { UserService } from 'src/app/core/authentication/services/user.service';
import { TEXT_TOKENS as content } from '../../../assets/i18n/_textTokens';
import { SuccessMessage } from '../shared/success-form-message/success-form-message.component';

export interface EditUserProps {
  user: User;
  firstNameLabel: string;
  lastNameLabel: string;
  emailLabel: string;
  passwordLabel: string;
  confirmPasswordLabel: string;
  editUserButtonLabel: string;
  userEditedMessage: string;
  userRolesDropdownLabel: String;
  userRoles: { label: string; role: string }[];
}

interface UpdateUserData {
  firstName: string;
  lastName: string;
  email: string;
  password: string;
  role: UserRole;
}

@Component({
  selector: 'app-edit-user',
  templateUrl: './edit-user.component.html',
  styleUrls: ['./edit-user.component.css'],
})
export class EditUserComponent implements OnChanges, OnInit {
  @Input() props: EditUserProps = this.getDefaultProps();

  @Output() submitEvent = new EventEmitter<unknown>();

  users: any;
  errorResponse: any;
  submitSuccessMessage: SuccessMessage = null;
  isLoading = false;
  userForm: FormGroup;
  validationErrors = content.validation_errors;
  userData?: User;
  showPasswordFields = false;

  firstNameArgs = { field: 'First Name', min: 2, max: 50 };
  lastNameArgs = { field: 'Last Name', min: 2, max: 50 };
  passwordArgs = { field: 'Password', min: 8, max: 50 };

  togglePasswordFields() {
    this.showPasswordFields = !this.showPasswordFields;
  }

  constructor(
    private userService: UserService,
    private route: ActivatedRoute,
  ) {
    this.userForm = new FormGroup(
      {
        firstName: new FormControl('', [Validators.required, Validators.minLength(this.firstNameArgs.min), Validators.maxLength(this.firstNameArgs.max)]),
        lastName: new FormControl('', [Validators.required, Validators.minLength(this.lastNameArgs.min), Validators.maxLength(this.lastNameArgs.max)]),
        email: new FormControl('', [Validators.required, Validators.email]),
        password: new FormControl('', [Validators.required, Validators.minLength(this.passwordArgs.min), Validators.maxLength(this.passwordArgs.max)]),
        confirmPassword: new FormControl('', Validators.required),
      },
      { validators: this.passwordMatchValidator },
    );
  }

  getDefaultProps(): EditUserProps {
    return {
      user: {
        firstName: '',
        lastName: '',
        email: '',
        role: UserRole.User,
      },
      firstNameLabel: 'First Name',
      lastNameLabel: 'Last Name',
      emailLabel: 'Email',
      passwordLabel: 'Password',
      confirmPasswordLabel: 'Confirm Password',

      editUserButtonLabel: 'Update User',
      userRolesDropdownLabel: 'Choose user role',
      userRoles: [],
      userEditedMessage: 'User Edited',
    };
  }

  ngOnChanges() {
    if (this.props.user) {
      this.userForm.patchValue({
        firstName: this.props.user.firstName,
        lastName: this.props.user.lastName,
        email: this.props.user.email,
        role: this.props.user.role,
      });
    }
  }

  ngOnInit(): void {
    this.userService.currentUser.subscribe({
      next: (currentUser: User | null) => {
        this.userData = currentUser!;

        if (this.userData) {
          this.userForm.patchValue({
            firstName: this.userData.firstName,
            lastName: this.userData.lastName,
            email: this.userData.email,
            role: this.userData.role,
          });
        } else {
          console.error('User data is null or undefined');
        }
      },
      error: (error) => {
        console.error('Error fetching current user data', error);
      },
    });
  }

  passwordMatchValidator: ValidatorFn = (control: AbstractControl): ValidationErrors | null => {
    const password = control.get('password');
    const confirmPassword = control.get('confirmPassword');

    return password && confirmPassword && password.value === confirmPassword.value ? null : { mismatch: true };
  };

  onSubmit() {
    if (!this.userData) {
      this.errorResponse = {
        error: {
          message: 'No user data',
        },
      };
      return;
    }

    this.isLoading = true;
    this.errorResponse = {};
    this.submitSuccessMessage = null;

    const userFormData: UpdateUserData = {
      firstName: this.userForm.value.firstName,
      lastName: this.userForm.value.lastName,
      email: this.userForm.value.email,
      password: this.userForm.value.password,
      role: this.userForm.value.role,
    };

    this.userService.update(userFormData).subscribe({
      next: (user) => {
        this.submitSuccessMessage = this.props.userEditedMessage;
        this.isLoading = false;
      },
      error: (error) => {
        console.error('user update error', error);
        this.errorResponse = error;
        this.isLoading = false;
      },
    });
  }
}
