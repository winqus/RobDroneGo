import mongoose from 'mongoose';
import { IDeliveryTaskPersistence } from '../../dataschema/IDeliveryTaskPersistence';

const DeliveryTask = new mongoose.Schema(
  {
    id: {
      type: String,
      unique: true,
    },

    pickUpRoomId: {
      type: String,
      required: [true, 'Please enter the pick up room id'],
    },

    deliveryRoomId: {
      type: String,
      required: [true, 'Please enter the delivery room id'],
    },

    pickUpContact: {
      type: Number,
      required: [true, 'Please enter the pick up contact'],
    },

    pickUpName: {
      type: String,
      required: [true, 'Please enter the pick up name'],
    },

    deliveryContact: {
      type: Number,
      required: [true, 'Please enter the delivery contact'],
    },

    deliveryName: {
      type: String,
      required: [true, 'Please enter the delivery name'],
    },

    confirmationCode: {
      type: Number,
      required: [true, 'Please enter the confirmation code'],
    },

    description: {
      type: String,
      required: [true, 'Please enter the description'],
    },
  },
  { timestamps: true },
);

export { DeliveryTask };

