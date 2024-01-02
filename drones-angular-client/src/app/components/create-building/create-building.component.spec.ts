import { ComponentFixture, TestBed, fakeAsync, tick, waitForAsync } from '@angular/core/testing';

import { CreateBuildingComponent } from './create-building.component';
import { ActivatedRoute, Router } from '@angular/router';
import { of } from 'rxjs';
import { ReactiveFormsModule } from '@angular/forms';
import BuildingService from '../../services/building.service';
import { StubFormErrorListComponent } from '../../../../cypress/utils/stubs/stub-form-error-list.component';
import { StubSuccessFormMessageComponent } from '../../../../cypress/utils/stubs/stub-success-form-message.component';


describe('CreateBuildingComponent', () => {
  let component: CreateBuildingComponent;
  let fixture: ComponentFixture<CreateBuildingComponent>;
  let buildingServiceSpy: jasmine.SpyObj<BuildingService>;
  let routerSpy: jasmine.SpyObj<Router>;
  let activatedRouteSpy: jasmine.SpyObj<ActivatedRoute>;

  beforeEach(waitForAsync(() => {
    const buildingServiceSpyObj = jasmine.createSpyObj('BuildingService', ['createBuilding']);
    const routerSpyObj = jasmine.createSpyObj('Router', ['navigate']);
    const activatedRouteSpyObj = jasmine.createSpyObj('ActivatedRoute', [], {
      queryParams: of({}),
    });

    TestBed.configureTestingModule({
      declarations: [CreateBuildingComponent, StubFormErrorListComponent, StubSuccessFormMessageComponent],
      imports: [ReactiveFormsModule],
      providers: [
        { provide: BuildingService, useValue: buildingServiceSpyObj },
        { provide: Router, useValue: routerSpyObj },
        { provide: ActivatedRoute, useValue: activatedRouteSpyObj },
      ],
    });

    buildingServiceSpy = TestBed.inject(BuildingService) as jasmine.SpyObj<BuildingService>;
    routerSpy = TestBed.inject(Router) as jasmine.SpyObj<Router>;
    activatedRouteSpy = TestBed.inject(ActivatedRoute) as jasmine.SpyObj<ActivatedRoute>;

    TestBed.compileComponents().then(() => {
    fixture = TestBed.createComponent(CreateBuildingComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
    });
  }));

  it('should create component', () => {
    expect(component).toBeTruthy();
  });

  it('should initialize form with default values', () => {
    const defaultValues = {
      name: '',
      code: '',
      description: '',
      floorSizeLength: '',
      floorSizeWidth: '',
    };

    expect(component.buildingForm.value).toEqual(defaultValues);
  });

  it('should submit building creation form', fakeAsync(() => {
    spyOn(component, 'onSubmit').and.stub();

    const buildingData = {
      name: 'Building 1',
      code: 'B1',
      description: 'Description for Building 1',
      floorSizeLength: 100,
      floorSizeWidth: 80,
    };

    component.buildingForm.setValue(buildingData);
    component.onSubmit();

    tick();

    expect(component.onSubmit).toHaveBeenCalled();
  }));
});
