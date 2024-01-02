import { ComponentFixture, TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { of } from 'rxjs';
import { UserRole } from '../../core/authentication/models/user-roles.enum';
import { User } from '../../core/authentication/models/user.model';
import { UserService } from '../../core/authentication/services/user.service';
import { UserListComponent } from './user-list.component';

describe('UserListComponent', () => {
  let component: UserListComponent;
  let fixture: ComponentFixture<UserListComponent>;
  let mockUserService: any;
  let mockUsers: User[];

  beforeEach(() => {
    mockUsers = [
      { id: '123', taxpayernumber: '', email: 'user1@example.com', firstName: 'John', lastName: 'Doe', phonenumber: '1234567890', role: UserRole.SystemAdministrator, isConfirmed: false },
      { id: '124', taxpayernumber: '', email: 'user2@example.com', firstName: 'Jane', lastName: 'Doe', phonenumber: '0987654321', role: UserRole.FleetManager, isConfirmed: true },
    ];

    mockUserService = jasmine.createSpyObj(['getAllUsers', 'confirmUser']);
    mockUserService.getAllUsers.and.returnValue(of(mockUsers));
    mockUserService.confirmUser.and.callFake((email, status) => {
      return of({ email, status });
    });

    TestBed.configureTestingModule({
      declarations: [UserListComponent],
      providers: [{ provide: UserService, useValue: mockUserService }],
    });

    fixture = TestBed.createComponent(UserListComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should load users on init', () => {
    component.users$!.subscribe((users) => {
      expect(users.length).toBe(2);
      expect(users).toEqual(mockUsers);
    });
  });

  it('should toggle user confirmation status', () => {
    const user = mockUsers[0];
    component.onConfirmToggle(user);
    expect(mockUserService.confirmUser).toHaveBeenCalledWith(user.email, user.isConfirmed);
    expect(user.isConfirmed).toBeTrue();
  });

  it('should display users correctly in the template', () => {
    fixture.detectChanges();
    const userElements = fixture.debugElement.queryAll(By.css('.grid-cols-6'));
    expect(userElements.length).toBe(3);
    expect(userElements[1].nativeElement.textContent).toContain(mockUsers[0].firstName);
    expect(userElements[2].nativeElement.textContent).toContain(mockUsers[1].firstName);
  });
});
