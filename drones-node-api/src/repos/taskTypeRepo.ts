import { Document, FilterQuery, Model } from 'mongoose';
import { Inject, Service } from 'typedi';
import { UniqueEntityID } from '../core/domain/UniqueEntityID';

import { ITaskTypePersistence } from '../dataschema/ITaskTypePersistence';
import { TaskType } from '../domain/TaskType/taskType';
import { TaskTypeMap } from '../mappers/TaskTypeMap';
import ITaskTypeRepo from '../services/IRepos/ITaskTypeRepo';

@Service()
export default class TaskTypeRepo implements ITaskTypeRepo {
  constructor(@Inject('taskTypeSchema') private taskTypeSchema: Model<ITaskTypePersistence & Document>) {}

  public async exists(taskType: TaskType): Promise<boolean> {
    const idX = taskType.id instanceof UniqueEntityID ? (<UniqueEntityID>taskType.id).toValue() : taskType.id;

    const query = { domainId: idX };
    const taskTypeDocument = await this.taskTypeSchema.findOne(query as FilterQuery<ITaskTypePersistence & Document>);

    return Boolean(taskTypeDocument) === true;
  }

  public async save(taskType: TaskType): Promise<TaskType> {
    const query = { id: taskType.id.toString() };

    const taskTypeDocument = await this.taskTypeSchema.findOne(query);

    try {
      if (taskTypeDocument === null) {
        const rawTaskType: any = TaskTypeMap.toPersistence(taskType);
        const taskTypeCreated = await this.taskTypeSchema.create(rawTaskType);

        return TaskTypeMap.toDomain(taskTypeCreated);
      } else {
        taskTypeDocument.type = taskType.type.toString();

        await taskTypeDocument.save();

        return taskType;
      }
    } catch (err) {
      throw err;
    }
  }

  public async findById(taskTypeId: string): Promise<TaskType> {
    const query = { id: taskTypeId };
    const taskTypeRecord = await this.taskTypeSchema.findOne(query as FilterQuery<ITaskTypePersistence & Document>);
    if (taskTypeRecord != null) {
      return TaskTypeMap.toDomain(taskTypeRecord);
    } else {
      return null;
    }
  }

  public async findByType(taskTypeType: string): Promise<TaskType> {
    const query = { type: taskTypeType };
    const taskTypeRecord = await this.taskTypeSchema.findOne(query as FilterQuery<ITaskTypePersistence & Document>);
    if (taskTypeRecord != null) {
      return TaskTypeMap.toDomain(taskTypeRecord);
    } else {
      return null;
    }
  }
}
