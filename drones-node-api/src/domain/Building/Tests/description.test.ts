import { Description } from '../ValueObjects/description';

describe('Description', () => {
  describe('create', () => {
    it('should successfully create a Description instance with valid strings', () => {
      // Arrange
      const validDescription = 'This is a valid description.';

      // Act
      const descriptionResult = Description.create(validDescription);

      // Assert
      expect(descriptionResult.isSuccess).toBe(true);
      expect(descriptionResult.getValue()).toBeInstanceOf(Description);
      expect(descriptionResult.getValue().value).toBe(validDescription.trim());
    });

    it('should fail if the description is null or undefined', () => {
      // Arrange
      const invalidDescription = null;

      // Act
      const descriptionResult = Description.create(invalidDescription);

      // Assert
      expect(descriptionResult.isFailure).toBe(true);
      expect(descriptionResult.error).toMatch(/null or undefined/);
    });

    it('should fail if the description length is more than 255 characters', () => {
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
      const descriptionWithSpaces = '    This is a description with spaces.    ';
      const expectedDescription = 'This is a description with spaces.';

      // Act
      const descriptionResult = Description.create(descriptionWithSpaces);

      // Assert
      expect(descriptionResult.isSuccess).toBe(true);
      expect(descriptionResult.getValue().value).toBe(expectedDescription);
    });
  });
});
