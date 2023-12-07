import mongoose from 'mongoose';
import { INavigationDataPersistence } from '../../dataschema/INavigationDataPersistence';

const NavigationData = new mongoose.Schema(
  {
    id: {
      type: String,
      unique: true,
    },

    floorsPaths: {
      type: [
        {
          fromBuilding: { type: String },
          fromFloorNumber: { type: String },
          toBuilding: { type: String },
          toFloorNumber: { type: String },
          type: { type: String },
        },
      ],
      required: [true, 'Please enter the floors paths'],
    },

    mapPathCount: {
      type: Number,
      required: [true, 'Please enter the map path count'],
    },

    mapPaths: {
      type: [
        {
          buildingCode: { type: String },
          cost: { type: Number },
          floorNumber: { type: Number },
          path: [{ col: { type: Number }, row: { type: Number } }],
        },
      ],
      required: [true, 'Please enter the map paths'],
    },
  },
  { timestamps: true },
);

export { NavigationData };

