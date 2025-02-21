import { Code } from '../ValueObjects/code';

describe('Code', () => {
  describe('create', () => {
    it.each(['A', 'A1b', 'a1B2C'])(
      'should successfully create a Code instance with valid alphanumeric strings of length between 1 to 5',
      (validCode) => {
        // Act
        const codeResult = Code.create(validCode);

        // Assert
        expect(codeResult.isSuccess).toBe(true);
        expect(codeResult.getValue()).toBeInstanceOf(Code);
        expect(codeResult.getValue().value).toBe(validCode.trim());
      },
    );

    it('should fail if the code is null or undefined', () => {
      // Arrange
      const invalidCode = null;

      // Act
      const codeResult = Code.create(invalidCode);

      // Assert
      expect(codeResult.isFailure).toBe(true);
      expect(codeResult.error).toMatch(/null or undefined/);
    });

    it('should fail if the code length is more than 5 characters', () => {
      // Arrange
      const longCode = 'ABC123';

      // Act
      const codeResult = Code.create(longCode);

      // Assert
      expect(codeResult.isFailure).toBe(true);
      expect(codeResult.error).toMatch(/between/);
    });

    it('should trim the code and succeed if it is otherwise valid', () => {
      // Arrange
      const codeWithSpaces = '  AB1  ';

      // Act
      const codeResult = Code.create(codeWithSpaces);

      // Assert
      expect(codeResult.isSuccess).toBe(true);
      expect(codeResult.getValue().value).toBe(codeWithSpaces.trim());
    });

    it.each(['A@1', 'b-2', 'AB!'])('should fail if the code contains special characters', (specialCharCode) => {
      // Act
      const codeResult = Code.create(specialCharCode);

      // Assert
      expect(codeResult.isFailure).toBe(true);
      expect(codeResult.error).toMatch(/alphanumeric/);
    });
  });
});
