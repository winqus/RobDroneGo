import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { Observable, of } from 'rxjs';

import { API_ROUTES } from '../../../api.config';
import { UserListComponent } from '../../components/user-list/user-list.component';
import { UserRole } from '../../core/authentication/models/user-roles.enum';
import { User } from '../../core/authentication/models/user.model';
import { UserService } from '../../core/authentication/services/user.service';

describe('UserListComponent Integration', () => {
  let component: UserListComponent;
  let fixture: ComponentFixture<UserListComponent>;
  let userService: UserService;
  let httpMock: HttpTestingController;

  let mockUsers: User[];

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [UserListComponent],
      imports: [HttpClientTestingModule],
      providers: [UserService],
    }).compileComponents();

    fixture = TestBed.createComponent(UserListComponent);
    component = fixture.componentInstance;
    userService = TestBed.inject(UserService);
    httpMock = TestBed.inject(HttpTestingController);

    mockUsers = [
      { id: '123', taxpayernumber: '', email: 'user1@example.com', firstName: 'John', lastName: 'Doe', phonenumber: '1234567890', role: UserRole.SystemAdministrator, isConfirmed: false },
      { id: '124', taxpayernumber: '', email: 'user2@example.com', firstName: 'Jane', lastName: 'Doe', phonenumber: '0987654321', role: UserRole.FleetManager, isConfirmed: true },
    ];
  });

  afterEach(() => {
    httpMock.verify();
  });

  it('should create the component and load users', () => {
    expect(component).toBeTruthy();

    component.ngOnInit();
    fixture.detectChanges();

    const req = httpMock.expectOne(API_ROUTES.user.getAll);
    expect(req.request.method).toBe('GET');
    req.flush(mockUsers);

    fixture.detectChanges();

    const userElements = fixture.debugElement.queryAll(By.css('.grid-cols-6')).slice(1, 3);
    expect(userElements.length).toEqual(mockUsers.length);
    expect(userElements[0].nativeElement.textContent).toContain(mockUsers[0].firstName);
    expect(userElements[1].nativeElement.textContent).toContain(mockUsers[1].firstName);
  });

  it("should toggle a user's confirmation status", () => {
    component.ngOnInit();
    fixture.detectChanges();

    const req = httpMock.expectOne(API_ROUTES.user.getAll);
    req.flush(mockUsers);
    fixture.detectChanges();

    const user = mockUsers[0];
    const toggleButton = fixture.debugElement.query(By.css(`input[type="checkbox"]`)).nativeElement;
    toggleButton.click();

    const confirmReq = httpMock.expectOne(API_ROUTES.user.confirm);
    expect(confirmReq.request.method).toBe('PATCH');
    expect(confirmReq.request.body).toEqual({ email: user.email, isConfirmed: true });
    confirmReq.flush({ ...user, isConfirmed: true });
    fixture.detectChanges();
  });

  it('should handle an empty user list', () => {
    component.ngOnInit();
    fixture.detectChanges();

    const req = httpMock.expectOne(API_ROUTES.user.getAll);
    req.flush([]);
    fixture.detectChanges();

    const userElements = fixture.debugElement.queryAll(By.css('.grid-cols-6'));
    expect(userElements.length).toBe(1);
  });
});
