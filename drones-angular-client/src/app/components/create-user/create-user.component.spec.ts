import { ComponentFixture, TestBed, fakeAsync, tick, waitForAsync } from '@angular/core/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { DomSanitizer } from '@angular/platform-browser';
import { Router } from '@angular/router';
import { of } from 'rxjs';
import { UserRole } from '../../core/authentication/models/user-roles.enum';
import { SystemAdminService } from '../../services/system-admin.service';
import { FormErrorListComponent } from '../form-error-list/form-error-list.component';
import { SuccessFormMessageComponent } from '../shared/success-form-message/success-form-message.component';
import { CreateUserComponent, CreateUserCredentials, CreateUserProps } from './create-user.component';

describe('CreateUserComponent', () => {
  let component: CreateUserComponent;
  let fixture: ComponentFixture<CreateUserComponent>;
  let systemAdminServiceSpy: jasmine.SpyObj<SystemAdminService>;

  const mockProps: CreateUserProps = {
    firstNameLabel: ' ',
    firstNamePlaceholder: ' ',
    lastNameLabel: ' ',
    lastNamePlaceholder: ' ',
    emailLabel: ' ',
    emailPlaceholder: ' ',
    phonenumberLabel: ' ',
    phonenumberPlaceholder: ' ',
    passwordLabel: ' ',
    passwordPlaceholder: ' ',
    confirmPasswordLabel: ' ',
    confirmPasswordPlaceholder: ' ',
    userRolesDropdownLabel: ' ',
    userRoles: { label: '', role: '' }[0],
    createUserButtonLabel: ' ',
    userCreatedMessage: ' ',
  };
  beforeEach(waitForAsync(() => {
    const systemAdminServiceSpyObj = jasmine.createSpyObj('SystemAdminService', ['createUser', 'confirmUser']);

    TestBed.configureTestingModule({
      declarations: [CreateUserComponent, FormErrorListComponent, SuccessFormMessageComponent],
      imports: [ReactiveFormsModule],
      providers: [
        { provide: DomSanitizer, useValue: {} },
        { provide: Router, useValue: {} },
        { provide: SystemAdminService, useValue: systemAdminServiceSpyObj },
      ],
    });

    systemAdminServiceSpy = TestBed.inject(SystemAdminService) as jasmine.SpyObj<SystemAdminService>;

    TestBed.compileComponents().then(() => {
      fixture = TestBed.createComponent(CreateUserComponent);
      component = fixture.componentInstance;
      fixture.detectChanges();
    });
  }));

  it('should create component', () => {
    expect(component).toBeTruthy();
  });

  it('should initialize form with default values', () => {
    const defaultValues: any = {
      firstName: '',
      lastName: '',
      email: '',
      phonenumber: '',
      password: '',
      confirmPassword: '',
      role: UserRole.CampusManager,
    };

    const formValues = component.createUserForm.value as CreateUserCredentials;

    expect(formValues).toEqual(defaultValues);
    expect(component.createUserForm.valid).toBeFalsy();
  });
});
