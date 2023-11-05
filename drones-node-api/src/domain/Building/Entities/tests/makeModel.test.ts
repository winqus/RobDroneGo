import { MakeModel } from '../ValueObjects/makeModel';

describe('MakeModel', () => {
  describe('create', () => {
    it('should successfully create a MakeModel instance with valid properties', () => {
      // Arrange
      const make = 'Brand';
      const model = 'Model';

      // Act
      const makeModelResult = MakeModel.create(make, model);

      // Assert
      expect(makeModelResult.isSuccess).toBe(true);
      const makeModel = makeModelResult.getValue();
      expect(makeModel).toBeInstanceOf(MakeModel);
      expect(makeModel.make).toBe(make);
      expect(makeModel.model).toBe(model);
    });

    it('should successfully create an empty MakeModel instance with null properties', () => {
      // Arrange
      const make = null;
      const model = null;

      // Act
      const makeModelResult = MakeModel.create(make, model);

      // Assert
      expect(makeModelResult.isSuccess).toBe(true);
      const makeModel = makeModelResult.getValue();
      expect(makeModel).toBeInstanceOf(MakeModel);
      expect(makeModel.make).toBe('');
      expect(makeModel.model).toBe('');
    });

    it('should successfully create an empty MakeModel instance with undefined properties', () => {
      // Arrange
      const make = undefined;
      const model = undefined;

      // Act
      const makeModelResult = MakeModel.create(make, model);

      // Assert
      expect(makeModelResult.isSuccess).toBe(true);
      const makeModel = makeModelResult.getValue();
      expect(makeModel).toBeInstanceOf(MakeModel);
      expect(makeModel.make).toBe('');
      expect(makeModel.model).toBe('');
    });

    it('should fail to create a MakeModel instance with make exceeding the maximum character limit', () => {
      // Arrange
      const longMake = 'A'.repeat(51);
      const model = 'Model';

      // Act
      const makeModelResult = MakeModel.create(longMake, model);

      // Assert
      expect(makeModelResult.isFailure).toBe(true);
      expect(makeModelResult.error).toMatch(/length/);
    });

    it('should fail to create a MakeModel instance with null make and valid model', () => {
      // Arrange
      const make = null;
      const model = 'Model';

      // Act
      const makeModelResult = MakeModel.create(make, model);

      // Assert
      expect(makeModelResult.isFailure).toBe(true);
      expect(makeModelResult.error).toMatch(/make/);
    });
  });
});
