import mongoose from 'mongoose';
import { IBuildingPersistence } from '../../dataschema/IBuildingPersistence';
import { elevatorSchema } from './elevatorSchema';

const Building = new mongoose.Schema(
  {
    id: {
      type: String,
      unique: true,
    },

    name: {
      type: String,
    },

    code: {
      type: String,
      required: [true, 'Please enter building code'],
      unique: true,
      index: true,
    },

    description: String,

    floorSizeLength: {
      type: Number,
      required: [true, 'Please enter building length'],
    },

    floorSizeWidth: {
      type: Number,
      required: [true, 'Please enter building width'],
    },
    elevator: {
      type: elevatorSchema,
    },
  },
  { timestamps: true },
);

export default mongoose.model<IBuildingPersistence & mongoose.Document>('Building', Building);
