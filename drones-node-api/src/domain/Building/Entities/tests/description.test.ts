import { Description } from '../ValueObjects/description';

describe('Description', () => {
  describe('create', () => {
    it('should successfully create a Description instance with a valid value', () => {
      // Arrange
      const validDescriptionValue = 'This is a valid description';

      // Act
      const descriptionResult = Description.create(validDescriptionValue);

      // Assert
      expect(descriptionResult.isSuccess).toBe(true);
      const description = descriptionResult.getValue();
      expect(description).toBeInstanceOf(Description);
      expect(description.value).toBe(validDescriptionValue);
    });

    it('should successfully create a Description instance with a trimmed valid value', () => {
      // Arrange
      const validDescriptionValue = '   This is a valid description with leading and trailing spaces   ';

      // Act
      const descriptionResult = Description.create(validDescriptionValue);

      // Assert
      expect(descriptionResult.isSuccess).toBe(true);
      const description = descriptionResult.getValue();
      expect(description).toBeInstanceOf(Description);
      expect(description.value).toBe('This is a valid description with leading and trailing spaces');
    });

    it('should successfully create an empty Description instance with null value', () => {
      // Arrange
      const nullDescriptionValue = null;

      // Act
      const descriptionResult = Description.create(nullDescriptionValue);

      // Assert
      expect(descriptionResult.isSuccess).toBe(true);
      const description = descriptionResult.getValue();
      expect(description).toBeInstanceOf(Description);
      expect(description.value).toBe('');
    });

    it('should successfully create an empty Description instance with undefined value', () => {
      // Arrange
      const undefinedDescriptionValue = undefined;

      // Act
      const descriptionResult = Description.create(undefinedDescriptionValue);

      // Assert
      expect(descriptionResult.isSuccess).toBe(true);
      const description = descriptionResult.getValue();
      expect(description).toBeInstanceOf(Description);
      expect(description.value).toBe('');
    });

    it('should fail to create a Description instance with a value longer than 250 characters', () => {
      // Arrange
      const longDescriptionValue = 'A'.repeat(251);

      // Act
      const descriptionResult = Description.create(longDescriptionValue);

      // Assert
      expect(descriptionResult.isFailure).toBe(true);
      expect(descriptionResult.error).toMatch(/length/);
    });
  });
});
