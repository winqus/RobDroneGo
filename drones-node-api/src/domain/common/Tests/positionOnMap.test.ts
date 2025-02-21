import { PositionOnMap } from '../positionOnMap';

describe('PositionOnMap', () => {
  it('should successfully create a PositionOnMap instance with valid coordinates', () => {
    const x = 1;
    const y = 2;
    const positionResult = PositionOnMap.create(x, y);
    expect(positionResult.isSuccess).toBe(true);
    expect(positionResult.getValue().value).toEqual({ x, y });
  });

  it('should fail for null or undefined coordinates', () => {
    const positionResult1 = PositionOnMap.create((null as unknown) as number, 1);
    const positionResult2 = PositionOnMap.create(1, (null as unknown) as number);
    expect(positionResult1.isFailure).toBe(true);
    expect(positionResult2.isFailure).toBe(true);
  });

  it('should fail for negative coordinates', () => {
    const positionResult1 = PositionOnMap.create(-1, 1);
    const positionResult2 = PositionOnMap.create(1, -1);
    expect(positionResult1.isFailure).toBe(true);
    expect(positionResult2.isFailure).toBe(true);
  });
});
