import { UniqueEntityID } from '../../core/domain/UniqueEntityID';
import { Code } from '../../domain/Building/ValueObjects/code';
import { DeliveryTask } from '../../domain/DeliveryTask/deliveryTask';
import { SurveillanceTask } from '../../domain/SurveillanceTask/surveillanceTask';
import { NavigationData } from '../../domain/TaskRequest/navigationData';
import { TaskRequest } from '../../domain/TaskRequest/taskRequest';
import { TaskStatus } from '../../domain/TaskRequest/taskStatus';
import { TaskRequestMap } from '../taskRequestMap';

describe('TaskRequestMap', () => {
  const date = new Date().toDateString();
  const id = '5f7f3b9d-3b7e-4b1e-8b4d-5b9f0a0e9e4e';

  const taskRequestDeliveryDTO = {
    id: id,
    status: Object.keys(TaskStatus).at(Object.values(TaskStatus).indexOf(TaskStatus.Pending)),
    requesterEmail: 'a@a.a',
    task: {
      id: id,
      pickUpRoomId: '5f8f3b9d-3b7e-4b1e-8b4d-5b9f0a0e9e4e',
      deliveryRoomId: '5f8f3b9d-3b7e-4b1e-8b4d-5b9f0a0e9e4a',
      pickUpContact: 911,
      pickUpName: 'a',
      deliveryContact: 912,
      deliveryName: 'b',
      confirmationCode: 123,
      description: 'c',
    },
    requestCreatedDateTime: date,
    navigationData: {
      floorsPaths: [
        {
          fromBuilding: 'a',
          fromFloorNumber: '1',
          toBuilding: 'b',
          toFloorNumber: '2',
          type: 'stairs',
        },
      ],
      mapPathCount: 1,
      mapPaths: [
        {
          buildingCode: 'a',
          cost: 1,
          floorNumber: 1,
          path: [
            {
              col: 1,
              row: 1,
            },
          ],
        },
      ],
    },
  };

  const taskRequestDelivery = TaskRequest.create(
    {
      status: TaskStatus.Pending,
      requesterEmail: 'a@a.a',
      task: DeliveryTask.create(
        {
          pickUpRoomId: new UniqueEntityID('5f8f3b9d-3b7e-4b1e-8b4d-5b9f0a0e9e4e'),
          deliveryRoomId: new UniqueEntityID('5f8f3b9d-3b7e-4b1e-8b4d-5b9f0a0e9e4a'),
          pickUpContact: 911,
          pickUpName: 'a',
          deliveryContact: 912,
          deliveryName: 'b',
          confirmationCode: 123,
          description: 'c',
        },
        new UniqueEntityID(id),
      ).getValue(),
      requestCreatedDateTime: new Date(date),
      navigationData: NavigationData.create({
        floorsPaths: [
          {
            fromBuilding: 'a',
            fromFloorNumber: '1',
            toBuilding: 'b',
            toFloorNumber: '2',
            type: 'stairs',
          },
        ],
        mapPathCount: 1,
        mapPaths: [
          {
            buildingCode: 'a',
            cost: 1,
            floorNumber: 1,
            path: [
              {
                col: 1,
                row: 1,
              },
            ],
          },
        ],
      }).getValue(),
    },
    new UniqueEntityID(id),
  ).getValue();

  const taskRequestSurveillanceDTO = {
    id: id,
    status: Object.keys(TaskStatus).at(Object.values(TaskStatus).indexOf(TaskStatus.Pending)),
    requesterEmail: 'a@a.a',
    task: {
      id: id,
      buildingCode: 'a',
      floorNumber: [1],
      contactNumber: 911,
    },
    requestCreatedDateTime: date,
    navigationData: {
      floorsPaths: [
        {
          fromBuilding: 'a',
          fromFloorNumber: '1',
          toBuilding: 'b',
          toFloorNumber: '2',
          type: 'stairs',
        },
      ],
      mapPathCount: 1,
      mapPaths: [
        {
          buildingCode: 'a',
          cost: 1,
          floorNumber: 1,
          path: [
            {
              col: 1,
              row: 1,
            },
          ],
        },
      ],
    },
  };

  const taskRequestSurveillance = TaskRequest.create(
    {
      status: TaskStatus.Pending,
      requesterEmail: 'a@a.a',
      task: SurveillanceTask.create(
        {
          buildingCode: Code.create('a').getValue(),
          floorNumber: [1],
          contactNumber: 911,
        },
        new UniqueEntityID(id),
      ).getValue(),
      requestCreatedDateTime: new Date(date),
      navigationData: NavigationData.create({
        floorsPaths: [
          {
            fromBuilding: 'a',
            fromFloorNumber: '1',
            toBuilding: 'b',
            toFloorNumber: '2',
            type: 'stairs',
          },
        ],
        mapPathCount: 1,
        mapPaths: [
          {
            buildingCode: 'a',
            cost: 1,
            floorNumber: 1,
            path: [
              {
                col: 1,
                row: 1,
              },
            ],
          },
        ],
      }).getValue(),
    },
    new UniqueEntityID(id),
  ).getValue();

  it('should map a TaskRequestDelivery to ITaskRequestDeliveryDTO', () => {
    expect(TaskRequestMap.toDTO(taskRequestDelivery)).toEqual(taskRequestDeliveryDTO);
    expect(TaskRequestMap.toDomain(taskRequestDeliveryDTO).getValue()).toEqual(taskRequestDelivery);
  });

  it('should map a TaskRequestSurveillance to ITaskRequestSurveillanceDTO', () => {
    expect(TaskRequestMap.toDTO(taskRequestSurveillance)).toEqual(taskRequestSurveillanceDTO);
    expect(TaskRequestMap.toDomain(taskRequestSurveillanceDTO).getValue()).toEqual(taskRequestSurveillance);
  });
});
