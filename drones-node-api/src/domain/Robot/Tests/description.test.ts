import { Description } from '../ValueObjects/description';

describe('Description', () => {
  describe('create', () => {
    it('should successfully create a Description instance with valid strings', () => {
      // Arrange
      const validDescription = 'This is a valid description';

      // Act
      const descriptionResult = Description.create(validDescription);

      // Assert
      expect(descriptionResult.isSuccess).toBe(true);
      expect(descriptionResult.getValue()).toBeInstanceOf(Description);
      expect(descriptionResult.getValue().value).toBe(validDescription.trim());
    });

    it.each([null, undefined, ''])(
      'should successfully create a Description instance with empty null or undefined',
      (validDescription) => {
        // Act
        const descriptionResult = Description.create(validDescription);

        // Assert
        expect(descriptionResult.isSuccess).toBe(true);
        expect(descriptionResult.getValue()).toBeInstanceOf(Description);
        expect(descriptionResult.getValue().value).toBe('');
      },
    );

    it('should fail if the description length is more than 250 characters', () => {
      // Arrange
      const longDescription = 'a'.repeat(256);

      // Act
      const descriptionResult = Description.create(longDescription);

      // Assert
      expect(descriptionResult.isFailure).toBe(true);
      expect(descriptionResult.error).toMatch(/between/);
    });

    it('should trim the description and succeed if it is otherwise valid', () => {
      // Arrange
      const descriptionWithSpaces = '    This is a description with spaces    ';
      const expectedDescription = 'This is a description with spaces';

      // Act
      const descriptionResult = Description.create(descriptionWithSpaces);

      // Assert
      expect(descriptionResult.isSuccess).toBe(true);
      expect(descriptionResult.getValue().value).toBe(expectedDescription);
    });
  });
});
