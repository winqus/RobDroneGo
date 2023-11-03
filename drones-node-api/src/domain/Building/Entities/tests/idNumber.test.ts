import { IDNumber } from '../ValueObjects/idNumber';

describe('IDNumber', () => {
  describe('create', () => {
    it('should successfully create an IDNumber instance with a valid number', () => {
      // Arrange
      const validNumber = 42;

      // Act
      const idNumberResult = IDNumber.create(validNumber);

      // Assert
      expect(idNumberResult.isSuccess).toBe(true);
      const idNumber = idNumberResult.getValue();
      expect(idNumber).toBeInstanceOf(IDNumber);
      expect(idNumber.value).toBe(validNumber);
    });

    it('should fail to create an IDNumber instance with a non-integer number', () => {
      // Arrange
      const nonIntegerNumber = 42.5;

      // Act
      const idNumberResult = IDNumber.create(nonIntegerNumber);

      // Assert
      expect(idNumberResult.isFailure).toBe(true);
      expect(idNumberResult.error).toMatch(/integer/);
    });

    it('should fail to create an IDNumber instance with a null value', () => {
      // Arrange
      const nullNumber = null;

      // Act
      const idNumberResult = IDNumber.create(nullNumber);

      // Assert
      expect(idNumberResult.isFailure).toBe(true);
      expect(idNumberResult.error).toMatch(/integer/);
    });

    it('should fail to create an IDNumber instance with an undefined value', () => {
      // Arrange
      const undefinedNumber = undefined;

      // Act
      const idNumberResult = IDNumber.create(undefinedNumber);

      // Assert
      expect(idNumberResult.isFailure).toBe(true);
      expect(idNumberResult.error).toMatch(/integer/);
    });
    it('should successfully create an IDNumber instance with a value of zero', () => {
      // Arrange
      const zeroValue = 0;

      // Act
      const idNumberResult = IDNumber.create(zeroValue);

      // Assert
      expect(idNumberResult.isSuccess).toBe(true);
      const idNumber = idNumberResult.getValue();
      expect(idNumber).toBeInstanceOf(IDNumber);
      expect(idNumber.value).toBe(zeroValue);
    });
  });
});
