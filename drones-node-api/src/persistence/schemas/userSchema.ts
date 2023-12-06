import mongoose from 'mongoose';
import { IUserPersistence } from '../../dataschema/IUserPersistence';

const User = new mongoose.Schema(
  {
    domainId: {
      type: String,
      unique: true,
      required: [true, 'Please enter domainId'],
    },

    firstName: {
      type: String,
      required: [true, 'Please enter first name'],
      index: true,
    },

    lastName: {
      type: String,
      required: [true, 'Please enter last name'],
      index: true,
    },

    email: {
      type: String,
      lowercase: true,
      unique: true,
      index: true,
      required: [true, 'Please enter email'],
    },

    phonenumber: String,

    taxpayernumber: String,

    password: String,

    salt: String,

    role: {
      type: String,
      required: [true, 'Please enter role'],
    },

    isConfirmed: {
      type: Boolean,
      default: false,
    },
  },
  { timestamps: true },
);

export default mongoose.model<IUserPersistence & mongoose.Document>('User', User);
