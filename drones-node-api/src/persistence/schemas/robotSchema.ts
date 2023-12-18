import mongoose from 'mongoose';
import IRobotPersistence from '../../dataschema/IRobotPersistence';

const Robot = new mongoose.Schema(
  {
    id: {
      type: String,
      unique: true,
    },

    code: {
      type: String,
      required: [true, 'Please enter robot code'],
      unique: true,
    },

    description: String,

    nickname: {
      type: String,
      required: [true, 'Please enter robot nickname'],
      unique: true,
    },

    serialNumber: {
      type: String,
      required: [true, 'Please enter robot serial number'],
    },

    available: {
      type: Boolean,
    },

    type: {
      type: String,
      required: [true, 'Please enter robot type'],
    },

    position: {
      floorNumber: {
        type: Number,
        default: 1,
      },
      buildingCode: {
        type: String,
        default: 'BCode',
      },
      cellPosition: {
        type: [Number, Number],
        default: [0, 0],
      },
    },
  },
  { timestamps: true, autoIndex: true },
);

Robot.index({ serialNumber: 1, type: 1 }, { unique: true });

export default mongoose.model<IRobotPersistence & mongoose.Document>('Robot', Robot);
