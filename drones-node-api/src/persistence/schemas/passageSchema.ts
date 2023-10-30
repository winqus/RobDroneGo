import mongoose from 'mongoose';
import { IPassagePersistence } from '../../dataschema/IPassagePersistence';

const Passage = new mongoose.Schema(
  {
    id: {
      type: String,
      unique: true,
    },

    buildingCode1: {
      type: String,
      required: [true, 'Please enter the building 1 code'],
    },

    buildingCode2: {
      type: String,
      required: [true, 'Please enter the building 2 code'],
    },

    floorNumber1: {
      type: Number,
      required: [true, 'Please enter the floor 1 number'],
    },

    floorNumber2: {
      type: Number,
      required: [true, 'Please enter the floor 2 number'],
    },
  },
  { timestamps: true },
);

export default mongoose.model<IPassagePersistence & mongoose.Document>('Passage', Passage);
