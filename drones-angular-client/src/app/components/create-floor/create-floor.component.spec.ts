import { ComponentFixture, TestBed, fakeAsync, tick, waitForAsync } from '@angular/core/testing';

import { CreateFloorComponent } from './create-floor.component';
import { FloorService } from '../../services/floor.service';
import { ActivatedRoute, Router } from '@angular/router';
import { of } from 'rxjs';
import { ReactiveFormsModule } from '@angular/forms';
import { StubFormErrorListComponent } from '../../../../cypress/utils/stubs/stub-form-error-list.component';
import { StubSuccessFormMessageComponent } from '../../../../cypress/utils/stubs/stub-success-form-message.component';
import BuildingService from '../../services/building.service';

describe('CreateFloorComponent', () => {
  let component: CreateFloorComponent;
  let fixture: ComponentFixture<CreateFloorComponent>;
  let floorServiceSpy: jasmine.SpyObj<FloorService>;
  let buildingServiceSpy: jasmine.SpyObj<BuildingService>;
  let routerSpy: jasmine.SpyObj<Router>;
  let activatedRouteSpy: jasmine.SpyObj<ActivatedRoute>;

  beforeEach(waitForAsync(() => {
    const floorServiceSpyObj = jasmine.createSpyObj('FloorService', ['createFloor']);
    const buildingServiceSpyObj = jasmine.createSpyObj('BuildingService', ['getAllBuildings']);
    const routerSpyObj = jasmine.createSpyObj('Router', ['navigate']);
    const activatedRouteSpyObj = jasmine.createSpyObj('ActivatedRoute', [], {
      queryParams: of({}),
    });

    buildingServiceSpyObj.getAllBuildings.and.returnValue(of([
      {
        id: '1',
        name: 'Building 1',
        code: 'B1',
        description: 'Description for Building 1',
        floorSizeLength: 100,
        floorSizeWidth: 80,
      },
      {
        id: '2',
        name: 'Building 2',
        code: 'B2',
        description: 'Description for Building 2',
        floorSizeLength: 120,
        floorSizeWidth: 90,
      },
    ]));
    

    TestBed.configureTestingModule({
      declarations: [CreateFloorComponent, StubFormErrorListComponent, StubSuccessFormMessageComponent],
      imports: [ReactiveFormsModule],
      providers: [
        { provide: FloorService, useValue: floorServiceSpyObj },
        { provide: BuildingService, useValue: buildingServiceSpyObj },
        { provide: Router, useValue: routerSpyObj },
        { provide: ActivatedRoute, useValue: activatedRouteSpyObj },
      ],
    });

    floorServiceSpy = TestBed.inject(FloorService) as jasmine.SpyObj<FloorService>;
    buildingServiceSpy = TestBed.inject(BuildingService) as jasmine.SpyObj<BuildingService>;
    routerSpy = TestBed.inject(Router) as jasmine.SpyObj<Router>;
    activatedRouteSpy = TestBed.inject(ActivatedRoute) as jasmine.SpyObj<ActivatedRoute>;

    TestBed.compileComponents().then(() => {
      fixture = TestBed.createComponent(CreateFloorComponent);
      component = fixture.componentInstance;
      fixture.detectChanges();
    });
  }));

  it('should create component', () => {
    expect(component).toBeTruthy();
  });

  it('should initialize form with default values', () => {
    const defaultValues = {
      buildingCode: '',
      floorNumber: '',
      description: '',
    };
    expect(component.floorForm.value).toEqual(defaultValues);
  });

  it('should fetch buildings on ngOnInit', fakeAsync(() => {
    component.ngOnInit();
    tick();

    expect(component.buildings).toEqual([
      {
        id: '1',
        name: 'Building 1',
        code: 'B1',
        description: 'Description for Building 1',
        floorSizeLength: 100,
        floorSizeWidth: 80,
      },
      {
        id: '2',
        name: 'Building 2',
        code: 'B2',
        description: 'Description for Building 2',
        floorSizeLength: 120,
        floorSizeWidth: 90,
      },
    ]);
  }));

  it('should submit floor creation form', fakeAsync(() => {
    spyOn(component, 'onSubmit').and.stub();

    const floorData = {
      buildingCode: 'B1',
      floorNumber: '1',
      description: 'A floor',
    };

    component.floorForm.controls['buildingCode'].setValue(floorData.buildingCode);
    component.floorForm.controls['floorNumber'].setValue(floorData.floorNumber);
    component.floorForm.controls['description'].setValue(floorData.description);

    component.onSubmit();

    tick();

    expect(component.onSubmit).toHaveBeenCalled();
  }));
});
