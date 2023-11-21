import mongoose from 'mongoose';

const mapSchema = new mongoose.Schema({
  size: {
    width: Number,
    height: Number,
  },
  map: [[Number]],
  exitLocations: {
    passages: [{ cellPosition: [Number, Number], destination: { buildingCode: String, floorNumber: Number } }],
    elevators: [{ cellPosition: [Number, Number] }],
  },
});

export { mapSchema };

