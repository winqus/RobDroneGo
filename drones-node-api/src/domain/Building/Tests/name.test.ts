import { Name } from '../ValueObjects/name';

describe('Name', () => {
  describe('create', () => {
    it('should successfully create a Name instance with valid alphanumeric strings of length between 1 to 50', () => {
      // Arrange
      const validNameString = 'BuildingA1';

      // Act
      const nameResult = Name.create(validNameString);

      // Assert
      expect(nameResult.isSuccess).toBe(true);
      expect(nameResult.getValue()).toBeInstanceOf(Name);
      expect(nameResult.getValue().value).toBe(validNameString);
    });

    it('should succesfully create an empty name with an empty or null value', () => {
      // Arrange
      const emptyNameString = '';
      const nullNameString = null;
      const undefinedNameString = undefined;

      // Act
      const nameResult1 = Name.create(emptyNameString);
      const nameResult2 = Name.create(nullNameString);
      const nameResult3 = Name.create(undefinedNameString);

      // Assert
      expect(nameResult1.isSuccess).toBe(true);
      expect(nameResult1.getValue()).toBeInstanceOf(Name);
      expect(nameResult1.getValue().value).toBe(emptyNameString);

      expect(nameResult2.isSuccess).toBe(true);
      expect(nameResult2.getValue()).toBeInstanceOf(Name);
      expect(nameResult2.getValue().value).toBe(emptyNameString);

      expect(nameResult3.isSuccess).toBe(true);
      expect(nameResult3.getValue()).toBeInstanceOf(Name);
      expect(nameResult3.getValue().value).toBe(emptyNameString);
    });

    it('should fail if the name string contains non-alphanumeric characters', () => {
      // Arrange
      const invalidNameString = 'Building A1!';

      // Act
      const nameResult = Name.create(invalidNameString);

      // Assert
      expect(nameResult.isFailure).toBe(true);
      expect(nameResult.error).toMatch(/alphanumeric/);
    });

    it('should fail if the name string length is more than 50 characters', () => {
      // Arrange
      const longNameString = 'a'.repeat(51);

      // Act
      const nameResult = Name.create(longNameString);

      // Assert
      expect(nameResult.isFailure).toBe(true);
      expect(nameResult.error).toMatch(/between/);
    });
  });
});
