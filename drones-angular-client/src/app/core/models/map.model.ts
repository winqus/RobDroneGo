export default interface Map {
  size: {
    width: number;
    height: number;
  };
  map: number[][];
  // passages: PassageCell[];
}

// export interface PassageCell {
//     cellPosition: [number, number];
//     destination: {
//         buildingCode: string;
//         floorNumber: number;
//     };
// }
