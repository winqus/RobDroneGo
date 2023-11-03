import { SerialNumber } from '../ValueObjects/serialNumber'; // Import the SerialNumber class

describe('SerialNumber', () => {
  describe('create', () => {
    it('should successfully create a SerialNumber instance with a valid value', () => {
      // Arrange
      const validSerialNumber = 'SN12345';

      // Act
      const serialNumberResult = SerialNumber.create(validSerialNumber);

      // Assert
      expect(serialNumberResult.isSuccess).toBe(true);
      const serialNumber = serialNumberResult.getValue();
      expect(serialNumber).toBeInstanceOf(SerialNumber);
      expect(serialNumber.value).toBe(validSerialNumber);
    });

    it('should successfully create an empty SerialNumber instance with null value', () => {
      // Arrange
      const nullSerialNumber = null;

      // Act
      const serialNumberResult = SerialNumber.create(nullSerialNumber);

      // Assert
      expect(serialNumberResult.isSuccess).toBe(true);
      const serialNumber = serialNumberResult.getValue();
      expect(serialNumber).toBeInstanceOf(SerialNumber);
      expect(serialNumber.value).toBe('');
    });

    it('should successfully create an empty SerialNumber instance with undefined value', () => {
      // Arrange
      const undefinedSerialNumber = undefined;

      // Act
      const serialNumberResult = SerialNumber.create(undefinedSerialNumber);

      // Assert
      expect(serialNumberResult.isSuccess).toBe(true);
      const serialNumber = serialNumberResult.getValue();
      expect(serialNumber).toBeInstanceOf(SerialNumber);
      expect(serialNumber.value).toBe('');
    });

    it('should fail to create a SerialNumber instance with a value longer than 50 characters', () => {
      // Arrange
      const longSerialNumber = 'A'.repeat(51);

      // Act
      const serialNumberResult = SerialNumber.create(longSerialNumber);

      // Assert
      expect(serialNumberResult.isFailure).toBe(true);
      expect(serialNumberResult.error).toMatch(/length/);
    });
  });
});
