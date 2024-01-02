import { ComponentFixture, TestBed, fakeAsync, tick, waitForAsync } from '@angular/core/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { ActivatedRoute } from '@angular/router';
import { of } from 'rxjs';
import { UserRole } from '../../core/authentication/models/user-roles.enum';
import { UserService } from '../../core/authentication/services/user.service';
import { FormErrorListComponent } from '../form-error-list/form-error-list.component';
import { SuccessFormMessageComponent } from '../shared/success-form-message/success-form-message.component';
import { EditUserComponent } from './edit-user.component';

describe('EditUserComponent', () => {
  let component: EditUserComponent;
  let fixture: ComponentFixture<EditUserComponent>;
  let userServiceSpy: jasmine.SpyObj<UserService>;

  const mockUser = {
    firstName: 'John',
    lastName: 'Doe',
    email: 'john.doe@example.com',
    phonenumber: '123456789',
    taxpayernumber: 'ABCDE12345',
    role: UserRole.User,
  };

  beforeEach(waitForAsync(() => {
    const userServiceSpyObj = jasmine.createSpyObj('UserService', ['update', 'deleteSelf', 'logout']);
    userServiceSpyObj.currentUser = of(mockUser);

    TestBed.configureTestingModule({
      declarations: [EditUserComponent, FormErrorListComponent, SuccessFormMessageComponent],
      imports: [ReactiveFormsModule],
      providers: [
        { provide: ActivatedRoute, useValue: { snapshot: { data: { user: mockUser } } } },
        { provide: UserService, useValue: userServiceSpyObj },
      ],
    });

    userServiceSpy = TestBed.inject(UserService) as jasmine.SpyObj<UserService>;

    TestBed.compileComponents().then(() => {
      fixture = TestBed.createComponent(EditUserComponent);
      component = fixture.componentInstance;
      fixture.detectChanges();
    });
  }));

  it('should delete user data', fakeAsync(() => {
    userServiceSpy.deleteSelf.and.returnValue(of({}));
    userServiceSpy.logout.and.stub();

    component.deleteUserData();

    tick();

    expect(userServiceSpy.deleteSelf).toHaveBeenCalled();
    expect(userServiceSpy.logout).toHaveBeenCalled();
  }));
});
