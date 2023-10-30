import { Code as BuildingCode } from '../../Building/ValueObjects/code';
import { Passage } from '../../Passage/passage';

describe('Passage', () => {
  describe('create', () => {
    it('should successfully create a Passage instance with valid properties', () => {
      // Arrange
      const passageProps = {
        buildingCode1: BuildingCode.create('BA').getValue(),
        buildingCode2: BuildingCode.create('BB').getValue(),
        floorNumber1: 1,
        floorNumber2: 1,
      };

      // Act
      const passageResult = Passage.create(passageProps);

      // Assert
      expect(passageResult.isSuccess).toBe(true);
      expect(passageResult.getValue()).toBeInstanceOf(Passage);
    });

    it('should fail to create a Passage instance with null properties', () => {
      // Arrange
      const passageProps = {
        buildingCode1: BuildingCode.create('BA').getValue(),
        buildingCode2: BuildingCode.create('BB').getValue(),
        floorNumber1: 3,
        floorNumber2: null,
      };

      // Act
      const passageResult = Passage.create(passageProps);

      // Assert
      expect(passageResult.isFailure).toBe(true);
      expect(passageResult.error).toMatch(/null/);
    });
  });
});
