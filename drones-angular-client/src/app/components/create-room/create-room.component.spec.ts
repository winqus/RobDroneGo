import { ComponentFixture, TestBed, fakeAsync, tick, waitForAsync } from '@angular/core/testing';

import { CreateRoomComponent } from './create-room.component';
import { ActivatedRoute, Router } from '@angular/router';
import { of } from 'rxjs';
import { ReactiveFormsModule } from '@angular/forms';
import { StubFormErrorListComponent } from '../../../../cypress/utils/stubs/stub-form-error-list.component';
import { StubSuccessFormMessageComponent } from '../../../../cypress/utils/stubs/stub-success-form-message.component';
import { RoomService } from '../../services/room.service';
import BuildingService from '../../services/building.service';
import { FloorService } from '../../services/floor.service';
import { floor } from 'lodash';

describe('CreateRoomComponent', () => {
  let component: CreateRoomComponent;
  let fixture: ComponentFixture<CreateRoomComponent>;
  let roomServiceSpy: jasmine.SpyObj<RoomService>;
  let buildingServiceSpy: jasmine.SpyObj<BuildingService>;
  let floorServiceSpy: jasmine.SpyObj<FloorService>;
  let routerSpy: jasmine.SpyObj<Router>;
  let activatedRouteSpy: jasmine.SpyObj<ActivatedRoute>;

  beforeEach(waitForAsync(() => {
    const roomServiceSpyObj = jasmine.createSpyObj('RoomService', ['createRoom']);
    const floorServiceSpyObj = jasmine.createSpyObj('FloorService', ['getAllFloors']);
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

    floorServiceSpyObj.getAllFloors.and.returnValue(of([
      {
        id: '1',
        floorNumber: 1,
        description: 'Description for Floor 1',
        servedByElevator: false,
        buildingCode: 'B1'
      },
    ]));

    TestBed.configureTestingModule({
      declarations: [CreateRoomComponent, StubFormErrorListComponent, StubSuccessFormMessageComponent],
      imports: [ReactiveFormsModule],
      providers: [
        { provide: RoomService, useValue: roomServiceSpyObj },
        { provide: BuildingService, useValue: buildingServiceSpyObj },
        { provide: Router, useValue: routerSpyObj },
        { provide: ActivatedRoute, useValue: activatedRouteSpyObj },
        { provide: FloorService, useValue: floorServiceSpyObj },
      ],
    });

    roomServiceSpy = TestBed.inject(RoomService) as jasmine.SpyObj<RoomService>;
    buildingServiceSpy = TestBed.inject(BuildingService) as jasmine.SpyObj<BuildingService>;
    floorServiceSpy = TestBed.inject(FloorService) as jasmine.SpyObj<FloorService>;
    routerSpy = TestBed.inject(Router) as jasmine.SpyObj<Router>;
    activatedRouteSpy = TestBed.inject(ActivatedRoute) as jasmine.SpyObj<ActivatedRoute>;

    TestBed.compileComponents().then(() => {
      fixture = TestBed.createComponent(CreateRoomComponent);
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
      name: '',
      description: '',
      size: {
        width: '',
        length: '',
      },
      position: {
        x: '',
        y: '',
      },
      category: '',
    };

    expect(component.roomForm.value).toEqual(defaultValues);
  });

  it('should load buildings on init', () => {
    expect(component.buildings.length).toBe(2);
  });

  it('should submit room creation form',fakeAsync(() => {
    spyOn(component, 'onSubmit').and.stub();

    const roomData = {
      buildingCode: 'B1',
      name: 'Room 1',
      description: 'Description for Room 1',
      size: {
        width: 10,
        length: 20,
      },
      position: {
        x: 10,
        y: 20,
      },
      category: 'Room',
    };

    component.roomForm.patchValue(roomData);

    component.onSubmit();

    tick();

    expect(component.onSubmit).toHaveBeenCalled();
  }));
});
