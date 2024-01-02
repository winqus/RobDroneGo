import { ComponentFixture, TestBed, fakeAsync, tick, waitForAsync } from '@angular/core/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { ActivatedRoute, Router } from '@angular/router';
import { of } from 'rxjs';
import { TaskFilters } from '../../core/models/shared/taskFilters.type';
import { TaskStatus } from '../../core/models/taskRequest.model';
import { TaskRequestService } from '../../services/task-request.service';
import { SearchTaskComponent, SearchTaskProps } from './search-task.component';

describe('SearchTaskComponent', () => {
  let component: SearchTaskComponent;
  let fixture: ComponentFixture<SearchTaskComponent>;
  let taskRequestServiceSpy: jasmine.SpyObj<TaskRequestService>;
  let routerSpy: jasmine.SpyObj<Router>;
  let activatedRouteSpy: jasmine.SpyObj<ActivatedRoute>;

  const mockProps: SearchTaskProps = {
    statusLabel: 'Status',
    statusPlaceholder: 'Enter task status',
    deviceLabel: 'Device',
    devicePlaceholder: 'Enter device',
    userLabel: 'User',
    userPlaceholder: 'Enter user email',
    searchResults: [],
    searchResultsPlaceholder: 'Search results',
    listTasksButtonLabel: 'List Tasks',
  };

  beforeEach(waitForAsync(() => {
    const taskRequestServiceSpyObj = jasmine.createSpyObj('TaskRequestService', ['getAllTaskRequests']);
    const routerSpyObj = jasmine.createSpyObj('Router', ['navigate']);
    const activatedRouteSpyObj = jasmine.createSpyObj('ActivatedRoute', [], {
      queryParams: of({}),
    });

    TestBed.configureTestingModule({
      declarations: [SearchTaskComponent],
      imports: [ReactiveFormsModule],
      providers: [
        { provide: TaskRequestService, useValue: taskRequestServiceSpyObj },
        { provide: Router, useValue: routerSpyObj },
        { provide: ActivatedRoute, useValue: activatedRouteSpyObj },
      ],
    });

    taskRequestServiceSpy = TestBed.inject(TaskRequestService) as jasmine.SpyObj<TaskRequestService>;
    routerSpy = TestBed.inject(Router) as jasmine.SpyObj<Router>;
    activatedRouteSpy = TestBed.inject(ActivatedRoute) as jasmine.SpyObj<ActivatedRoute>;

    TestBed.compileComponents().then(() => {
      fixture = TestBed.createComponent(SearchTaskComponent);
      component = fixture.componentInstance;
      fixture.detectChanges();
    });
  }));

  it('should create component', () => {
    expect(component).toBeTruthy();
  });

  it('should initialize form with default values', () => {
    const defaultValues = {
      status: '',
      device: '',
      user: '',
      searchResults: '',
    };

    const formValues = component.searchTaskForm.value;

    expect(formValues).toEqual(defaultValues);
  });

  it('should submit form and navigate with queryParams', fakeAsync(() => {
    spyOn(component, 'getTasksByFilter').and.stub();

    const queryParams: Partial<TaskFilters> = {
      status: TaskStatus.Approved,
      robotTypeName: 'Robot123',
      userEmail: 'user@example.com',
    };

    component.searchTaskForm.controls['status'].setValue(TaskStatus.Approved);
    component.searchTaskForm.controls['device'].setValue('Robot123');
    component.searchTaskForm.controls['user'].setValue('user@example.com');

    component.searchTaskForm.patchValue(queryParams);

    component.onSubmit();

    tick();

    expect(component.getTasksByFilter).toHaveBeenCalledWith(queryParams as TaskFilters);
  }));
});
