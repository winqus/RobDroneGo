import { Brand } from '../ValueObjects/brand'; // Import the Brand class

describe('Brand', () => {
  describe('create', () => {
    it('should successfully create a Brand instance with valid properties', () => {
      // Arrange
      const brandValue = 'ExampleBrand';

      // Act
      const brandResult = Brand.create(brandValue);

      // Assert
      expect(brandResult.isSuccess).toBe(true);
      expect(brandResult.getValue()).toBeInstanceOf(Brand);
    });

    it('should fail to create a Brand instance with null value', () => {
      // Arrange
      const brandValue = null;

      // Act
      const brandResult = Brand.create(brandValue);

      // Assert
      expect(brandResult.isFailure).toBe(true);
      expect(brandResult.error).toMatch(/null/);
    });

    it('should fail to create a Brand instance with non-alphanumeric value', () => {
      // Arrange
      const brandValue = '@InvalidBrand#';

      // Act
      const brandResult = Brand.create(brandValue);

      // Assert
      expect(brandResult.isFailure).toBe(true);
      expect(brandResult.error).toMatch(/alphanumeric/);
    });

    it('should fail to create a Brand instance with a value longer than 50 characters', () => {
      // Arrange
      const brandValue = 'A'.repeat(51);

      // Act
      const brandResult = Brand.create(brandValue);

      // Assert
      expect(brandResult.isFailure).toBe(true);
      expect(brandResult.error).toMatch(/length/);
    });

    it('should successfully create a Brand instance with a 50-character value', () => {
      // Arrange
      const brandValue = 'A'.repeat(50);

      // Act
      const brandResult = Brand.create(brandValue);

      // Assert
      expect(brandResult.isSuccess).toBe(true);
      expect(brandResult.getValue()).toBeInstanceOf(Brand);
    });

    it('should successfully create a Brand instance with a 1-character value', () => {
      // Arrange
      const brandValue = 'A';

      // Act
      const brandResult = Brand.create(brandValue);

      // Assert
      expect(brandResult.isSuccess).toBe(true);
      expect(brandResult.getValue()).toBeInstanceOf(Brand);
    });
  });
});
