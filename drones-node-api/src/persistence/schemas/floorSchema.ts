import mongoose from 'mongoose';
import { IFloorPersistence } from '../../dataschema/IFloorPersistence';

const Floor = new mongoose.Schema(
  {
    id: {
      type: String,
      unique: true,
    },

    code: {
      type: String,
      required: [true, 'Please enter floor code'],
      unique: true,
      index: true,
    },

    description: String,

    servedByElevator: Boolean,

    buildingCode: {
      type: String,
      required: [true, 'Please enter building code'],
    },
  },
  { timestamps: true },
);

export default mongoose.model<IFloorPersistence & mongoose.Document>('Floor', Floor);
