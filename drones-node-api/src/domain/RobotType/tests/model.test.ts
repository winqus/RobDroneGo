import { Model } from '../ValueObjects/model'; // Import the Model class

describe('Model', () => {
  describe('create', () => {
    it('should successfully create a Model instance with valid properties', () => {
      // Arrange
      const modelValue = 'ExampleModel';

      // Act
      const modelResult = Model.create(modelValue);

      // Assert
      expect(modelResult.isSuccess).toBe(true);
      expect(modelResult.getValue()).toBeInstanceOf(Model);
    });

    it('should fail to create a Model instance with null value', () => {
      // Arrange
      const modelValue = null;

      // Act
      const modelResult = Model.create(modelValue);

      // Assert
      expect(modelResult.isFailure).toBe(true);
      expect(modelResult.error).toMatch(/null/);
    });

    it('should fail to create a Model instance with non-alphanumeric value', () => {
      // Arrange
      const modelValue = '@InvalidModel#';

      // Act
      const modelResult = Model.create(modelValue);

      // Assert
      expect(modelResult.isFailure).toBe(true);
      expect(modelResult.error).toMatch(/alphanumeric/);
    });

    it('should fail to create a Model instance with a value longer than 100 characters', () => {
      // Arrange
      const modelValue = 'A'.repeat(101);

      // Act
      const modelResult = Model.create(modelValue);

      // Assert
      expect(modelResult.isFailure).toBe(true);
      expect(modelResult.error).toMatch(/length/);
    });

    it('should successfully create a Model instance with a 100-character value', () => {
      // Arrange
      const modelValue = 'A'.repeat(100);

      // Act
      const modelResult = Model.create(modelValue);

      // Assert
      expect(modelResult.isSuccess).toBe(true);
      expect(modelResult.getValue()).toBeInstanceOf(Model);
    });

    it('should successfully create a Model instance with a 1-character value', () => {
      // Arrange
      const modelValue = 'A';

      // Act
      const modelResult = Model.create(modelValue);

      // Assert
      expect(modelResult.isSuccess).toBe(true);
      expect(modelResult.getValue()).toBeInstanceOf(Model);
    });
  });
});
