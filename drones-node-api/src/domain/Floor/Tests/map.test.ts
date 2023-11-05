import { Map } from '../ValueObject/map';

describe('Map', () => {
  it('should create a valid map', () => {
    const width = 3;
    const height = 2;
    const mapData = [
      [0, 1, 2],
      [3, 4, 5],
    ];

    const mapResult = Map.create(width, height, mapData);

    expect(mapResult.isSuccess).toBe(true);
    const map = mapResult.getValue();
    expect(map.width).toBe(width);
    expect(map.height).toBe(height);
    expect(map.map).toEqual(mapData);
  });

  it('should fail to create a map with invalid width', () => {
    const width = 0;
    const height = -1;
    const mapData = [
      [0, 1],
      [2, 3],
    ];

    const mapResult = Map.create(width, height, mapData);

    expect(mapResult.isFailure).toBe(true);
    expect(mapResult.error).toBe('width must be greater than 0');
  });

  it('should fail to create a map with non-integer width', () => {
    const width = 2.5;
    const height = 3;
    const mapData = [
      [0, 1],
      [2, 3],
    ];

    const mapResult = Map.create(width, height, mapData);

    expect(mapResult.isFailure).toBe(true);
    expect(mapResult.error).toBe('width should be an integer');
  });

  it('should fail to create a map with empty array map data', () => {
    const width = 2;
    const height = 2;
    const mapData = [[]];

    const mapResult = Map.create(width, height, mapData);

    expect(mapResult.isFailure).toBe(true);
    expect(mapResult.error).toBe('map height must match height');
  });
});
