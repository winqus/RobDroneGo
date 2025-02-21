import { FloorSize } from '../ValueObjects/floorSize';

describe('FloorSize', () => {
  describe('create', () => {
    it('should successfully create a FloorSize instance with valid length and width', () => {
      // Arrange
      const validLength = 10;
      const validWidth = 20;

      // Act
      const floorSizeResult = FloorSize.create(validLength, validWidth);

      // Assert
      expect(floorSizeResult.isSuccess).toBe(true);
      expect(floorSizeResult.getValue()).toBeInstanceOf(FloorSize);
      expect(floorSizeResult.getValue().value).toEqual({ length: validLength, width: validWidth });
    });

    it('should fail if length or width is null or undefined', () => {
      // Arrange
      const invalidLength = null;
      const invalidWidth = undefined;

      // Act
      const floorSizeResult1 = FloorSize.create(invalidLength, 10);
      const floorSizeResult2 = FloorSize.create(10, invalidWidth);

      // Assert
      expect(floorSizeResult1.isFailure).toBe(true);
      expect(floorSizeResult1.error).toMatch(/null or undefined/);
      expect(floorSizeResult2.isFailure).toBe(true);
      expect(floorSizeResult2.error).toMatch(/null or undefined/);
    });

    it('should fail if length or width is not greater than 0', () => {
      // Arrange
      const zeroLength = 0;
      const negativeWidth = -5;

      // Act
      const floorSizeResult1 = FloorSize.create(zeroLength, 10);
      const floorSizeResult2 = FloorSize.create(10, negativeWidth);

      // Assert
      expect(floorSizeResult1.isFailure).toBe(true);
      expect(floorSizeResult1.error).toMatch(/greater than 0/);
      expect(floorSizeResult2.isFailure).toBe(true);
      expect(floorSizeResult2.error).toMatch(/greater than 0/);
    });
  });
});
