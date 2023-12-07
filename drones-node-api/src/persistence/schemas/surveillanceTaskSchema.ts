import mongoose from 'mongoose';
import { ISurveillanceTaskPersistence } from '../../dataschema/ISurveillanceTaskPersistence';

const SurveillanceTask = new mongoose.Schema(
  {
    id: {
      type: String,
      unique: true,
    },

    buildingCode: {
      type: String,
      required: [true, 'Please enter the building code'],
    },

    floorNumber: {
      type: [Number],
      required: [true, 'Please enter the floor number'],
    },

    contactNumber: {
      type: Number,
      required: [true, 'Please enter the contact number'],
    },
  },
  { timestamps: true },
);

export { SurveillanceTask };

