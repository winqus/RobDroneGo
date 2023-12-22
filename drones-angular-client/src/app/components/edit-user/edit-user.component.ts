import { Component, EventEmitter, Input, OnChanges, OnInit, Output } from '@angular/core';
import { AbstractControl, FormControl, FormGroup, ValidationErrors, ValidatorFn, Validators } from '@angular/forms';
import { ActivatedRoute } from '@angular/router';
import { TEXT_TOKENS as content } from '../../../assets/i18n/_textTokens';
import { environment } from '../../../environments/environment';
import { UserRole } from '../../core/authentication/models/user-roles.enum';
import { User } from '../../core/authentication/models/user.model';
import { UserService } from '../../core/authentication/services/user.service';
import { SuccessMessage } from '../shared/success-form-message/success-form-message.component';

export interface EditUserProps {
  user: User;
  firstNameLabel: string;
  lastNameLabel: string;
  phonenumberLabel: string;
  passwordLabel: string;
  confirmPasswordLabel: string;
  editUserButtonLabel: string;
  downloadUserButtonLabel: string;
  deleteUserButtonLabel: string;
  userEditedMessage: string;
  userDeletedMessage: string;
  userRolesDropdownLabel: String;
  userRoles: { label: string; role: string }[];
}

interface UpdateUserData {
  firstName: string;
  lastName: string;
  phonenumber: string;
  password: string;
}

@Component({
  selector: 'app-edit-user',
  templateUrl: './edit-user.component.html',
  styleUrls: ['./edit-user.component.css'],
})
export class EditUserComponent implements OnChanges, OnInit {
  @Input() props: Partial<EditUserProps> = this.getDefaultProps();

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
  router: any;

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
        phonenumber: new FormControl('', [Validators.required, Validators.pattern('^[0-9]{9}$')]),
        password: new FormControl('', [Validators.required, Validators.minLength(10), Validators.pattern(/^(?=.*[A-Z])(?=.*[a-z])(?=.*\d)(?=.*[@$!%*?&.,#^+])[A-Za-z\d@$!%*?&.,#^+]{10,}$/)]),
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
        phonenumber: '',
        taxpayernumber: '',
        role: UserRole.User,
      } as User,
      firstNameLabel: 'First Name',
      lastNameLabel: 'Last Name',
      phonenumberLabel: 'Phone number',
      passwordLabel: 'Password',
      confirmPasswordLabel: 'Confirm Password',
      downloadUserButtonLabel: 'Download',

      editUserButtonLabel: 'Update User',
      userEditedMessage: 'User Edited',
      userDeletedMessage: 'User Deleted',
      userRolesDropdownLabel: 'User Role',
      userRoles: [],

      deleteUserButtonLabel: 'Delete user',
    };
  }

  ngOnChanges() {
    if (this.props.user) {
      this.userForm.patchValue({
        firstName: this.props.user.firstName,
        lastName: this.props.user.lastName,
        phonenumber: this.props.user.phonenumber,
        taxpayernumber: this.props.user.taxpayernumber,
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
            phonenumber: this.userData.phonenumber,
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

    const updatedUserData: UpdateUserData = {
      firstName: this.userForm.value.firstName,
      lastName: this.userForm.value.lastName,
      phonenumber: this.userForm.value.phonenumber,
      password: this.userForm.value.password || undefined,
    };

    this.userService.update(updatedUserData).subscribe({
      next: (user) => {
        this.submitSuccessMessage = this.props.userEditedMessage || null;
        this.isLoading = false;
      },
      error: (error) => {
        console.error('user update error', error);
        this.errorResponse = error;
        this.isLoading = false;
      },
    });
  }

  downloadUserData() {
    const userDataForDownload = {
      firstName: this.userData!.firstName,
      lastName: this.userData!.lastName,
      email: this.userData!.email,
      phoneNumber: this.userData!.phonenumber,
      taxpayerNumber: this.userData!.taxpayernumber || undefined,
    };
    const userDataJson = JSON.stringify(userDataForDownload, null, 2); // The '2' here adds indentation to the JSON string

    const blob = new Blob([userDataJson], { type: 'application/json' });

    const link = document.createElement('a');
    link.href = window.URL.createObjectURL(blob);
    link.download = 'userData.json';
    link.click();
  }
  deleteUserData() {
    this.userService.deleteSelf().subscribe({
      next: () => {
        this.userService.logout();
      },
      error: (error) => {
        console.error('user delete error', error);
      },
    });
  }
}
