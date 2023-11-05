import { SerialNumber } from '../ValueObjects/serialNumber';

describe('SerialNumber', () => {
  describe('create', () => {
    it('should successfully create a Serial Number instance with valid strings', () => {
      // Arrange
      const validNumber = 'This is valid';

      // Act
      const numberResult = SerialNumber.create(validNumber);

      // Assert
      expect(numberResult.isSuccess).toBe(true);
      expect(numberResult.getValue()).toBeInstanceOf(SerialNumber);
      expect(numberResult.getValue().value).toBe(validNumber.trim());
    });

    it.each([null, undefined, ''])(
      'should fail to create a Serial Number instance with empty null or undefined',
      (invalidNumber) => {
        // Act
        const numberResult = SerialNumber.create(invalidNumber);

        // Assert
        expect(numberResult.isFailure).toBe(true);
        expect(numberResult.error).toMatch(
          /(serialNumber can only contain alphanumeric characters and spaces|serialNumber is null or undefined)/,
        );
      },
    );

    it('should fail if the serial number length is more than 50 characters', () => {
      // Arrange
      const longNumber = 'a'.repeat(60);

      // Act
      const numberResult = SerialNumber.create(longNumber);

      // Assert
      expect(numberResult.isFailure).toBe(true);
      expect(numberResult.error).toMatch(/between/);
    });

    it('should trim the serial number and succeed if it is otherwise valid', () => {
      // Arrange
      const numberWithSpaces = '    This is a number    ';
      const expectedNumber = 'This is a number';

      // Act
      const numberResult = SerialNumber.create(numberWithSpaces);

      // Assert
      expect(numberResult.isSuccess).toBe(true);
      expect(numberResult.getValue().value).toBe(expectedNumber);
    });
  });
});
