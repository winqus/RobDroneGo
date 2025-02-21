import { Inject, Service } from 'typedi';

import { Document, FilterQuery, Model } from 'mongoose';
import { UniqueEntityID } from '../core/domain/UniqueEntityID';
import { ITaskRequestPersistence } from '../dataschema/ITaskRequestPersistence';
import { DeliveryTask } from '../domain/DeliveryTask/deliveryTask';
import { TaskRequest } from '../domain/TaskRequest/taskRequest';
import { TaskStatus } from '../domain/TaskRequest/taskStatus';
import { DeliveryTaskMap } from '../mappers/deliveryTaskMap';
import { NavigationDataMap } from '../mappers/navigationDataMap';
import { SurveillanceTaskMap } from '../mappers/surveillanceTaskMap';
import { TaskRequestMap } from '../mappers/taskRequestMap';
import ITaskRequestRepo from '../services/IRepos/ITaskRequestRepo';

@Service()
export default class TaskRequestRepo implements ITaskRequestRepo {
  static taskRequestSchema: any;
  constructor(@Inject('taskRequestSchema') private taskRequestSchema: Model<ITaskRequestPersistence & Document>) {}

  public async exists(taskRequest: TaskRequest): Promise<boolean> {
    const idX = taskRequest.id instanceof UniqueEntityID ? (<UniqueEntityID>taskRequest.id).toValue() : taskRequest.id;

    const query = { domainId: idX };
    const taskRequestDocument = await this.taskRequestSchema.findOne(
      query as FilterQuery<ITaskRequestPersistence & Document>,
    );

    return Boolean(taskRequestDocument) === true;
  }

  public async getById(taskRequestId: string): Promise<TaskRequest> {
    const query = { id: taskRequestId };
    const taskRequestRecord = await this.taskRequestSchema.findOne(
      query as FilterQuery<ITaskRequestPersistence & Document>,
    );

    if (taskRequestRecord != null) {
      return TaskRequestMap.toDomain(taskRequestRecord).getValue();
    } else {
      return null;
    }
  }

  public async getAll(): Promise<TaskRequest[]> {
    const taskRequestRecords = await this.taskRequestSchema.find({});

    return taskRequestRecords.map((record) => TaskRequestMap.toDomain(record).getValue());
  }

  public async save(taskRequest: TaskRequest): Promise<TaskRequest> {
    const query = { id: taskRequest.id.toString() };

    const taskRequestDocument = await this.taskRequestSchema.findOne(query);

    try {
      if (taskRequestDocument === null) {
        const rawTaskRequest: any = TaskRequestMap.toPersistence(taskRequest);
        const taskRequestRecord = await this.taskRequestSchema.create(rawTaskRequest);

        return TaskRequestMap.toDomain(taskRequestRecord).getValue();
      } else {
        taskRequestDocument.status = Object.keys(TaskStatus).at(Object.values(TaskStatus).indexOf(taskRequest.status));
        taskRequestDocument.requesterEmail = taskRequest.requesterEmail;
        // taskRequestDocument.task = TaskMap.toPersistence(taskRequest.task);
        taskRequestDocument.task =
          taskRequest.task instanceof DeliveryTask
            ? DeliveryTaskMap.toPersistence(taskRequest.task)
            : SurveillanceTaskMap.toPersistence(taskRequest.task);
        taskRequestDocument.requestCreatedDateTime = taskRequest.requestCreatedDateTime.toString();
        taskRequestDocument.navigationData = taskRequest.navigationData
          ? NavigationDataMap.toPersistence(taskRequest.navigationData)
          : null;

        await taskRequestDocument.save();

        return taskRequest;
      }
    } catch (error) {
      throw error;
    }
  }
}
