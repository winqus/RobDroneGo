import { ComponentFixture, TestBed } from '@angular/core/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { of } from 'rxjs';
import { UserRole } from '../../core/authentication/models/user-roles.enum';
import { UserService } from '../../core/authentication/services/user.service';
import BuildingService from '../../services/building.service';
import { FloorService } from '../../services/floor.service';
import { RoomService } from '../../services/room.service';
import { TaskRequestService } from '../../services/task-request.service';
import { FormErrorListComponent } from '../form-error-list/form-error-list.component';
import { SuccessFormMessageComponent } from '../shared/success-form-message/success-form-message.component';
import { CreateTaskRequestComponent } from './create-task-request.component';

describe('CreateTaskRequestComponent', () => {
  let component: CreateTaskRequestComponent;
  let fixture: ComponentFixture<CreateTaskRequestComponent>;
  let userServiceMock: any;
  let buildingServiceMock: any;
  let floorServiceMock: any;
  let roomServiceMock: any;
  let taskRequestServiceMock: any;

  const userStub = {
    id: '1',
    firstName: 'John',
    lastName: 'Doe',
    email: 'john@mail.mock',
    phonenumber: '123456789',
    taxpayernumber: '123456789',
    role: UserRole.User,
    isConfirmed: true,
  };

  beforeEach(() => {
    userServiceMock = jasmine.createSpyObj(['getCurrentUser']);
    buildingServiceMock = jasmine.createSpyObj(['getAllBuildings']);
    floorServiceMock = jasmine.createSpyObj(['getFloorsByBuildingCode']);
    roomServiceMock = jasmine.createSpyObj(['getAllRooms']);
    taskRequestServiceMock = jasmine.createSpyObj(['createTaskRequest']);

    TestBed.configureTestingModule({
      imports: [ReactiveFormsModule, FormsModule],
      declarations: [CreateTaskRequestComponent, FormErrorListComponent, SuccessFormMessageComponent],
      providers: [
        { provide: UserService, useValue: userServiceMock },
        { provide: BuildingService, useValue: buildingServiceMock },
        { provide: FloorService, useValue: floorServiceMock },
        { provide: RoomService, useValue: roomServiceMock },
        { provide: TaskRequestService, useValue: taskRequestServiceMock },
      ],
    });

    fixture = TestBed.createComponent(CreateTaskRequestComponent);
    component = fixture.componentInstance;
    buildingServiceMock.getAllBuildings.and.returnValue(of([]));
    userServiceMock.getCurrentUser.and.returnValue(of({ user: userStub }));
    roomServiceMock.getAllRooms.and.returnValue(of([]));
    fixture.detectChanges();
  });

  it('should initialize the component', () => {
    expect(component).toBeTruthy();
    expect(component.deliveryForm).toBeDefined();
    expect(component.surveillanceForm).toBeDefined();
    expect(userServiceMock.getCurrentUser).toHaveBeenCalled();
    expect(buildingServiceMock.getAllBuildings).toHaveBeenCalled();
    expect(roomServiceMock.getAllRooms).toHaveBeenCalled();
  });

  it('roomMatchValidator should set sameRoom error if pickup and delivery rooms are the same', () => {
    const form = component.deliveryForm;
    form.get('pickUpRoomId')!.setValue('Room1');
    form.get('deliveryRoomId')!.setValue('Room1');
    const errors = component.roomMatchValidator(form);
    expect(errors).toEqual({ sameRoom: true });
    expect(form.get('deliveryRoomId')!.errors).toEqual({ sameRoom: true });
  });

  it('should submit a delivery task', () => {
    taskRequestServiceMock.createTaskRequest.and.returnValue(of({}));
    component.taskType = 'Delivery';
    component.deliveryForm.setValue({
      pickUpBuildingCode: 'B',
      pickUpFloorNumber: 1,
      pickUpRoomId: '1',
      deliveryBuildingCode: 'B',
      deliveryFloorNumber: 2,
      deliveryRoomId: '1',
      pickUpContact: '123456789',
      pickUpName: 'John',
      deliveryContact: '123456789',
      deliveryName: 'John',
      confirmationCode: '1234',
      description: 'Description',
    });
    component.submitTask();
    expect(taskRequestServiceMock.createTaskRequest).toHaveBeenCalled();
  });

  it('should submit a surveillance task', () => {
    taskRequestServiceMock.createTaskRequest.and.returnValue(of({}));
    component.taskType = 'Surveillance';
    component.surveillanceForm.setValue({
      buildingCode: 'B',
      floorNumber: [1, 2],
      contactNumber: '123456789',
    });
    component.submitTask();
    expect(taskRequestServiceMock.createTaskRequest).toHaveBeenCalled();
  });
});
