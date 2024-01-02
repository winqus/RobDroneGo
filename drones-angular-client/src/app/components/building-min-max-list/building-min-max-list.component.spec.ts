import { ComponentFixture, TestBed, tick } from '@angular/core/testing';

import { FormGroup, ReactiveFormsModule } from '@angular/forms';
import { RouterTestingModule } from '@angular/router/testing';
import { of, throwError } from 'rxjs';
import Building from '../../core/models/building.model';
import Floor from '../../core/models/floor.model';
import BuildingService from '../../services/building.service';
import { FloorService } from '../../services/floor.service';
import { FormErrorListComponent } from '../form-error-list/form-error-list.component';
import { SuccessFormMessageComponent } from '../shared/success-form-message/success-form-message.component';
import { BuildingMinMaxListComponent } from './building-min-max-list.component';

describe('BuildingMinMaxListComponent', () => {
  let component: BuildingMinMaxListComponent;
  let fixture: ComponentFixture<BuildingMinMaxListComponent>;
  let mockBuildingService: jasmine.SpyObj<BuildingService>;
  let mockFloorService: jasmine.SpyObj<FloorService>;

  let mockFloors: Floor[] = [];
  let mockBuildings: Building[];

  beforeEach(() => {
    mockBuildingService = jasmine.createSpyObj('BuildingService', ['getBuildingByCode']);
    mockFloorService = jasmine.createSpyObj('FloorService', ['getAllFloors']);

    mockFloors = [
      {
        id: '1',
        floorNumber: 1,
        description: 'Floor 1',
        servedByElevator: true,
        buildingCode: 'B1',
      },
    ];

    mockBuildings = [
      {
        id: '1',
        name: 'Building 1',
        code: 'B1',
        description: 'Building 1',
        floorSizeLength: 10,
        floorSizeWidth: 10,
      },
    ];

    TestBed.configureTestingModule({
      declarations: [BuildingMinMaxListComponent, FormErrorListComponent, SuccessFormMessageComponent],
      imports: [ReactiveFormsModule, RouterTestingModule],
      providers: [
        { provide: BuildingService, useValue: mockBuildingService },
        { provide: FloorService, useValue: mockFloorService },
      ],
    });

    fixture = TestBed.createComponent(BuildingMinMaxListComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should initialize form with empty values and required validators', () => {
    const form: FormGroup = component.buildingMinMaxForm;
    const minFloorControl = form.get('minFloor')!;
    const maxFloorControl = form.get('maxFloor')!;

    expect(minFloorControl.value).toEqual('');
    expect(maxFloorControl.value).toEqual('');
    expect(minFloorControl.errors!['required']).toBeTruthy();
    expect(maxFloorControl.errors!['required']).toBeTruthy();
  });

  it('should validate min and max floor inputs correctly', () => {
    component.buildingMinMaxForm.setValue({ minFloor: 2, maxFloor: 5 });
    expect(component.buildingMinMaxForm.valid).toBeTruthy();
    component.buildingMinMaxForm.setValue({ minFloor: 6, maxFloor: 5 });
    expect(component.buildingMinMaxForm.invalid).toBeTruthy();
  });

  it('should filter buildings correctly on getBuildings', () => {
    mockFloorService.getAllFloors.and.returnValue(of(mockFloors));
    mockBuildingService.getBuildingByCode.and.returnValue(of(mockBuildings[0]));

    component.getBuildings(1, 2);
    fixture.detectChanges();

    expect(component.buildings.length).toBe(1);
    expect(component.buildings).toEqual(mockBuildings);
    expect(component.noBuildingsFoundMessage).toEqual('');
  });

  it('should set noBuildingsFoundMessage if no buildings are found', () => {
    mockFloorService.getAllFloors.and.returnValue(of(mockFloors));
    mockBuildingService.getBuildingByCode.and.returnValue(of(mockBuildings[0]));
    component.getBuildings(6, 7);
    expect(component.noBuildingsFoundMessage).toEqual('No buildings found within the given range');
  });

  it('should emit submitEvent on form submission', () => {
    component.buildingMinMaxForm.setValue({ minFloor: 2, maxFloor: 5 });
    mockFloorService.getAllFloors.and.returnValue(of([]));
    spyOn(component.submitEvent, 'emit');

    component.onSubmit();

    expect(component.buildingMinMaxForm.valid).toBeTruthy();
    expect(mockFloorService.getAllFloors).toHaveBeenCalled();
  });
});
