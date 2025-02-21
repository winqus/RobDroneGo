import mongoose from 'mongoose';
import { IRoomPersistence } from '../../dataschema/IRoomPersistence';

const Room = new mongoose.Schema(
  {
    id: {
      type: String,
      unique: true,
    },
    name: {
      type: String,
      required: [true, 'Please enter the name'],
    },
    description: {
      type: String,
    },
    size: {
      width: {
        type: Number,
        required: [true, 'Please enter the width'],
      },
      length: {
        type: Number,
        required: [true, 'Please enter the length'],
      },
    },
    position: {
      x: {
        type: Number,
        required: [true, 'Please enter the x position'],
      },
      y: {
        type: Number,
        required: [true, 'Please enter the y position'],
      },
    },
    category: {
      type: String,
      required: [true, 'Please enter the category'],
    },
    floorId: {
      type: String,
      required: [true, 'Please enter the floor id'],
    },
  },
  { timestamps: true },
);

export default mongoose.model<IRoomPersistence & mongoose.Document>('Room', Room);
