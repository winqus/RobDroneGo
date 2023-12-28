import { ComponentFixture, TestBed } from '@angular/core/testing';

import { By } from '@angular/platform-browser';
import { Router } from '@angular/router';
import { RouterTestingModule } from '@angular/router/testing';
import { of } from 'rxjs';
import { TaskRequest, TaskStatus } from '../../core/models/taskRequest.model';
import { TaskRequestService } from '../../services/task-request.service';
import { ListTaskRequestComponent } from './list-task-request.component';

describe('ListTaskRequestComponent', () => {
  let component: ListTaskRequestComponent;
  let fixture: ComponentFixture<ListTaskRequestComponent>;
  let mockTaskRequestService: any;
  let mockRouter: any;
  const mockTaskRequests: TaskRequest[] = [
    {
      id: 'tr-01',
      status: TaskStatus.Pending,
      requesterEmail: 'email1@email.com',
      task: {
        pickUpRoomId: 'room-01',
        deliveryRoomId: 'room-02',
        pickUpContact: 123456789,
        pickUpName: 'name1',
        deliveryContact: 987654321,
        deliveryName: 'name2',
        confirmationCode: 1234,
        description: 'description1',
      },
      requestCreatedDateTime: 'today',
      navigationData: undefined,
    },
    {
      id: 'tr-02',
      status: TaskStatus.Pending,
      requesterEmail: 'email2',
      task: {
        pickUpRoomId: 'room-03',
        deliveryRoomId: 'room-04',
        pickUpContact: 123456789,
        pickUpName: 'name3',
        deliveryContact: 987654321,
        deliveryName: 'name4',
        confirmationCode: 1234,
        description: 'description2',
      },
      requestCreatedDateTime: 'today',
      navigationData: undefined,
    },
    {
      id: 'tr-03',
      status: TaskStatus.Pending,
      requesterEmail: 'email3',
      task: {
        buildingCode: 'B1',
        floorNumber: [1, 2, 3],
        contactNumber: 123456789,
      },
      requestCreatedDateTime: 'today',
      navigationData: undefined,
    },
    {
      id: 'tr-04',
      status: TaskStatus.Approved,
      requesterEmail: 'email4',
      task: {
        buildingCode: 'B2',
        floorNumber: [1, 2, 3],
        contactNumber: 123456789,
      },
      requestCreatedDateTime: 'today',
      navigationData: undefined,
    },
    {
      id: 'tr-05',
      status: TaskStatus.Approved,
      requesterEmail: 'email5',
      task: {
        pickUpRoomId: 'room-05',
        deliveryRoomId: 'room-06',
        pickUpContact: 123456789,
        pickUpName: 'name5',
        deliveryContact: 987654321,
        deliveryName: 'name6',
        confirmationCode: 1234,
        description: 'description5',
      },
      requestCreatedDateTime: 'today',
      navigationData: {
        floorsConnectionsCost: 100,
        floorsPaths: [
          {
            fromBuilding: 'B',
            fromFloorNumber: '1',
            toBuilding: 'B',
            toFloorNumber: '2',
            type: 'Elevator',
          },
        ],
        mapPathCount: 3,
        mapPaths: [
          {
            buildingCode: 'B',
            cost: 100,
            floorNumber: 1,
            path: [
              { col: 1, row: 1 },
              { col: 2, row: 2 },
            ],
          },
          {
            buildingCode: 'B',
            cost: 100,
            floorNumber: 2,
            path: [
              { col: 1, row: 1 },
              { col: 2, row: 2 },
            ],
          },
          {
            buildingCode: 'B',
            cost: 100,
            floorNumber: 3,
            path: [
              { col: 1, row: 1 },
              { col: 2, row: 2 },
            ],
          },
        ],
      },
    },
  ];

  beforeEach(() => {
    mockTaskRequestService = jasmine.createSpyObj(['getAllTaskRequests', 'updateTaskRequestStatus']);
    mockRouter = jasmine.createSpyObj('Router', ['navigate']);

    TestBed.configureTestingModule({
      declarations: [ListTaskRequestComponent],
      providers: [
        { provide: TaskRequestService, useValue: mockTaskRequestService },
        { provide: Router, useValue: mockRouter },
      ],
      imports: [RouterTestingModule],
    });

    fixture = TestBed.createComponent(ListTaskRequestComponent);
    component = fixture.componentInstance;
    mockTaskRequestService.getAllTaskRequests.and.returnValue(of(mockTaskRequests));
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should load all TaskRequest on init', () => {
    expect(component.taskRequestsFullData.length).toBe(mockTaskRequests.length);
    expect(component.taskRequestsFullData).toEqual(mockTaskRequests);
  });

  it('should say is delivery', () => {
    const taskRequest = mockTaskRequests[0];
    const bool = component.isDelivery(taskRequest);
    expect(bool).toBeTrue();
  });

  it('should say is not delivery', () => {
    const taskRequest = mockTaskRequests[2];
    const bool = component.isDelivery(taskRequest);
    expect(bool).toBeFalse();
  });

  it('should say that have navigationData', () => {
    const taskRequest = mockTaskRequests[4];
    const bool = component.hasNavigationData(taskRequest);
    expect(bool).toBeTrue();
  });

  it('should say not have navigationData', () => {
    const taskRequest = mockTaskRequests[0];
    const bool = component.hasNavigationData(taskRequest);
    expect(bool).toBeFalse();
  });

  it('should say approve the taskRequest', () => {
    const taskRequestApprove = mockTaskRequests[0];
    taskRequestApprove.status = TaskStatus.Approved;
    mockTaskRequestService.updateTaskRequestStatus.and.returnValue(of(taskRequestApprove));
    const taskRequest = mockTaskRequests[0];
    component.approveRequest(taskRequest);
    expect(mockTaskRequestService.updateTaskRequestStatus).toHaveBeenCalledWith(taskRequest.id, TaskStatus.Approved);
    expect(taskRequest.status).toEqual(TaskStatus.Approved);
  });
});
