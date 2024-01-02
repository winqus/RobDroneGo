import { ComponentFixture, TestBed, fakeAsync, tick, waitForAsync } from '@angular/core/testing';

import { CreateRobotComponent } from './create-robot.component';
import { RobotService } from '../../services/robot.service';
import { ActivatedRoute, Router } from '@angular/router';
import { of } from 'rxjs';
import { ReactiveFormsModule } from '@angular/forms';
import { StubFormErrorListComponent } from '../../../../cypress/utils/stubs/stub-form-error-list.component';
import { StubSuccessFormMessageComponent } from '../../../../cypress/utils/stubs/stub-success-form-message.component';


describe('CreateRobotComponent', () => {
  let component: CreateRobotComponent;
  let fixture: ComponentFixture<CreateRobotComponent>;
  let robotServiceSpy: jasmine.SpyObj<RobotService>;
  let routerSpy: jasmine.SpyObj<Router>;
  let activatedRouteSpy: jasmine.SpyObj<ActivatedRoute>;

  beforeEach(waitForAsync(() => {
    const robotServiceSpyObj = jasmine.createSpyObj('RobotService', ['createRobot']);
    const routerSpyObj = jasmine.createSpyObj('Router', ['navigate']);
    const activatedRouteSpyObj = jasmine.createSpyObj('ActivatedRoute', [], {
      queryParams: of({}),
    });

    TestBed.configureTestingModule({
      declarations: [CreateRobotComponent, StubFormErrorListComponent,
      StubSuccessFormMessageComponent, ],
      imports: [ReactiveFormsModule],
      providers: [
        { provide: RobotService, useValue: robotServiceSpyObj },
        { provide: Router, useValue: routerSpyObj },
        { provide: ActivatedRoute, useValue: activatedRouteSpyObj },
      ],
    });

    robotServiceSpy = TestBed.inject(RobotService) as jasmine.SpyObj<RobotService>;
    routerSpy = TestBed.inject(Router) as jasmine.SpyObj<Router>;
    activatedRouteSpy = TestBed.inject(ActivatedRoute) as jasmine.SpyObj<ActivatedRoute>;

    TestBed.compileComponents().then(() => {
    fixture = TestBed.createComponent(CreateRobotComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
    });
  }));

  it('should create component', () => {
    expect(component).toBeTruthy();
  });

  it('should initialize form with default values', () => {
    const defaultValues = {
      code: '',
      nickname: '',
      serialNumber: '',
      description: '',
      type: '',
    };

    expect(component.robotForm.value).toEqual(defaultValues);
  });

  it('should submit robot creation form', fakeAsync(() => {
    spyOn(component, 'onSubmit').and.stub();
  
    const robotData = {
      code: 'Robot123',
      nickname: 'Bot',
      serialNumber: 'SN123',
      type: 'Service Bot',
      description: 'A helpful service robot.',
    };
  
    component.robotForm.controls['code'].setValue(robotData.code);
    component.robotForm.controls['nickname'].setValue(robotData.nickname);
    component.robotForm.controls['serialNumber'].setValue(robotData.serialNumber);
    component.robotForm.controls['type'].setValue(robotData.type);
    component.robotForm.controls['description'].setValue(robotData.description);
  
    component.onSubmit();
  
    tick();
  
    expect(component.onSubmit).toHaveBeenCalled();

  }));
  

});
