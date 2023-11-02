import mongoose from 'mongoose';
import { IElevatorPersistence } from '../../dataschema/IElevatorPersistence';

const elevatorSchema = new mongoose.Schema({
  id: {
    type: String,
  },
  number: {
    type: Number,
    required: [true, 'Please enter elevator number'],
  },
  make: {
    type: String,
  },
  model: {
    type: String,
  },
  serialNumber: {
    type: String,
  },
  description: {
    type: String,
  },
});

export { elevatorSchema };

