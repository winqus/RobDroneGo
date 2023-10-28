import mongoose from 'mongoose';
import { IFloorPersistence } from '../../dataschema/IFloorPersistence';

const Floor = new mongoose.Schema(
  {
    id: {
      type: String,
      unique: true,
    },

    floorNumber: {
      type: Number,
      required: [true, 'Please enter floor number'],
    },

    description: String,

    servedByElevator: Boolean,

    buildingCode: {
      type: String,
      required: [true, 'Please enter building code'],
    },
  },
  { timestamps: true, autoIndex: true },
);

Floor.index({ floorNumber: 1, buildingCode: 1 }, { unique: true });

export default mongoose.model<IFloorPersistence & mongoose.Document>('Floor', Floor);
