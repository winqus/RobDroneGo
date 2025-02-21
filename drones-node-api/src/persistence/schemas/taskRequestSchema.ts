import mongoose, { Schema } from 'mongoose';
import { ITaskRequestPersistence } from '../../dataschema/ITaskRequestPersistence';
import { NavigationData } from './navigationDataSchema';

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
      type: Schema.Types.Mixed,
      required: [true, 'Please enter the task'],
    },

    requestCreatedDateTime: {
      type: Date,
      required: [true, 'Please enter the request created date time'],
    },

    navigationData: {
      type: NavigationData,
    },
  },
  { timestamps: true },
);

export default mongoose.model<ITaskRequestPersistence & mongoose.Document>('TaskRequest', TaskRequest);
