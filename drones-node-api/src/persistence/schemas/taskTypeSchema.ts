import mongoose from 'mongoose';
import { ITaskTypePersistence } from '../../dataschema/ITaskTypePersistence';

const TaskType = new mongoose.Schema(
  {
    id: {
      type: String,
      unique: true,
    },

    type: {
      type: String,
    },
  },
  { timestamps: true },
);

export default mongoose.model<ITaskTypePersistence & mongoose.Document>('TaskType', TaskType);
