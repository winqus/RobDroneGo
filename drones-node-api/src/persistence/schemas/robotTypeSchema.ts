import mongoose from 'mongoose';
import { IRobotTypePersistence } from '../../dataschema/IRobotTypePersistence';

const RobotType = new mongoose.Schema(
  {
    id: {
      type: String,
      unique: true,
    },

    name: {
      type: String,
      unique: true,
    },

    brand: {
      type: String,
    },

    model: {
      type: String,
    },

    typesOfTasks: {
      type: [String],
    },
  },
  { timestamps: true },
);

RobotType.index({ brand: 1, model: 1 }, { unique: true });

export default mongoose.model<IRobotTypePersistence & mongoose.Document>('RobotType', RobotType);
