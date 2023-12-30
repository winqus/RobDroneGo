import { ComponentFixture, TestBed, fakeAsync, tick, waitForAsync } from '@angular/core/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { of } from 'rxjs';
import { StubFormErrorListComponent } from '../../../../cypress/utils/stubs/stub-form-error-list.component';
import { StubSuccessFormMessageComponent } from '../../../../cypress/utils/stubs/stub-success-form-message.component';
import Building from '../../core/models/building.model';
import Floor from '../../core/models/floor.model';
import BuildingService from '../../services/building.service';
import { ElevatorService } from '../../services/elevator.service';
import { FloorService } from '../../services/floor.service';
import { CreateElevatorComponent } from './create-elevator.component';

describe('CreateElevatorComponent', () => {
  let component: CreateElevatorComponent;
  let fixture: ComponentFixture<CreateElevatorComponent>;
  let elevatorServiceSpy: jasmine.SpyObj<ElevatorService>;
  let buildingServiceSpy: jasmine.SpyObj<BuildingService>;
  let floorServiceSpy: jasmine.SpyObj<FloorService>;

  const buildingsMock: Building[] = [
    {
      id: 'building123',
      name: null,
      code: 'building123',
      description: 'Building 123',
      floorSizeLength: 100,
      floorSizeWidth: 100,
      elevator: undefined,
    },
  ];

  beforeEach(waitForAsync(() => {
    const elevatorServiceSpyObj = jasmine.createSpyObj('ElevatorService', ['createElevator']);
    const buildingServiceSpyObj = jasmine.createSpyObj('BuildingService', ['getAllBuildings']);
    const floorServiceSpyObj = jasmine.createSpyObj('FloorService', ['getFloorsByBuildingCode']);

    TestBed.configureTestingModule({
      declarations: [CreateElevatorComponent, StubSuccessFormMessageComponent, StubFormErrorListComponent],
      imports: [ReactiveFormsModule],
      providers: [
        { provide: ElevatorService, useValue: elevatorServiceSpyObj },
        { provide: BuildingService, useValue: buildingServiceSpyObj },
        { provide: FloorService, useValue: floorServiceSpyObj },
      ],
    });

    elevatorServiceSpy = TestBed.inject(ElevatorService) as jasmine.SpyObj<ElevatorService>;
    buildingServiceSpy = TestBed.inject(BuildingService) as jasmine.SpyObj<BuildingService>;
    floorServiceSpy = TestBed.inject(FloorService) as jasmine.SpyObj<FloorService>;

    TestBed.compileComponents().then(() => {
      fixture = TestBed.createComponent(CreateElevatorComponent);
      component = fixture.componentInstance;
      fixture.detectChanges();
    });

    buildingServiceSpy.getAllBuildings.and.returnValue(of(buildingsMock));
  }));

  it('should create component', () => {
    expect(component).toBeTruthy();
  });

  it('should initialize elevatorForm with default values', () => {
    const defaultValues = {
      buildingCode: '',
      selectedFloors: [],
      number: '',
      make: '',
      model: '',
      serialNumber: '',
      description: '',
    };

    expect(component.elevatorForm.value).toEqual(defaultValues);
  });

  it('should load buildings on initialization', () => {
    component.ngOnInit();

    expect(component.buildings).toEqual(buildingsMock);
  });

  it('should load floors when buildingCode changes', () => {
    const buildingCode = 'building123';
    const floorsMock: Floor[] = [
      {
        id: 'floor123',
        floorNumber: 1,
        description: 'Floor 123',
        servedByElevator: false,
        buildingCode: 'building123',
        map: undefined,
      },
    ];
    floorServiceSpy.getFloorsByBuildingCode.and.returnValue(of(floorsMock));

    component.elevatorForm.get('buildingCode')?.setValue(buildingCode);
    component.loadFloors(buildingCode);

    expect(component.floors).toEqual(floorsMock);
  });
});
