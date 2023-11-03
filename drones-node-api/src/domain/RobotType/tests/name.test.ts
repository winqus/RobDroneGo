import { Name } from '../ValueObjects/name'; // Import the Name class

describe('Name', () => {
  describe('create', () => {
    it('should successfully create a Name instance with valid properties', () => {
      // Arrange
      const nameValue = 'John Doe';

      // Act
      const nameResult = Name.create(nameValue);

      // Assert
      expect(nameResult.isSuccess).toBe(true);
      expect(nameResult.getValue()).toBeInstanceOf(Name);
    });

    it('should fail to create a Name instance with null value', () => {
      // Arrange
      const nameValue = null;

      // Act
      const nameResult = Name.create(nameValue);

      // Assert
      expect(nameResult.isFailure).toBe(true);
      expect(nameResult.error).toMatch(/null/);
    });

    it('should fail to create a Name instance with non-alphanumeric characters', () => {
      // Arrange
      const nameValue = 'John123#';

      // Act
      const nameResult = Name.create(nameValue);

      // Assert
      expect(nameResult.isFailure).toBe(true);
      expect(nameResult.error).toMatch(/alphanumeric/);
    });

    it('should fail to create a Name instance with a value longer than 25 characters', () => {
      // Arrange
      const nameValue = 'A'.repeat(26);

      // Act
      const nameResult = Name.create(nameValue);

      // Assert
      expect(nameResult.isFailure).toBe(true);
      expect(nameResult.error).toMatch(/length/);
    });

    it('should successfully create a Name instance with a 25-character value', () => {
      // Arrange
      const nameValue = 'A'.repeat(25);

      // Act
      const nameResult = Name.create(nameValue);

      // Assert
      expect(nameResult.isSuccess).toBe(true);
      expect(nameResult.getValue()).toBeInstanceOf(Name);
    });

    it('should successfully create a Name instance with a 1-character value', () => {
      // Arrange
      const nameValue = 'A';

      // Act
      const nameResult = Name.create(nameValue);

      // Assert
      expect(nameResult.isSuccess).toBe(true);
      expect(nameResult.getValue()).toBeInstanceOf(Name);
    });
  });
});
