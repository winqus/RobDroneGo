import mongoose from 'mongoose';
import { ITaskRequestPersistence } from '../../dataschema/ITaskRequestPersistence';
import { DeliveryTask } from './deliveryTaskSchema';
import { NavigationData } from './navigationDataSchema';
import { SurveillanceTask } from './surveillanceTaskSchema';

const TaskRequest = new mongoose.Schema(
  {
    id: {
      type: String,
      unique: true,
    },

    status: {
      type: String,
      required: [true, 'Please enter the status'],
    },

    requesterEmail: {
      type: String,
      required: [true, 'Please enter the requester email'],
    },

    task: {
      type: DeliveryTask || SurveillanceTask,
      required: [true, 'Please enter the task'],
    },

    requestCreatedDateTime: {
      type: Date,
      required: [true, 'Please enter the request created date time'],
    },

    navigationData: {
      type: NavigationData,
      required: [true, 'Please enter the navigation data'],
    },
  },
  { timestamps: true },
);

export default mongoose.model<ITaskRequestPersistence & mongoose.Document>('TaskRequest', TaskRequest);
