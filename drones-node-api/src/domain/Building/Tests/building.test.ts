import { Code } from '../ValueObjects/code';
import { Description } from '../ValueObjects/description';
import { FloorSize } from '../ValueObjects/floorSize';
import { Name } from '../ValueObjects/name';
import { Building } from '../building';

describe('Building', () => {
  describe('create', () => {
    it('should successfully create a Building instance with valid properties', () => {
      // Arrange
      const buildingProps = {
        name: Name.create('Building1').getValue(),
        code: Code.create('A1').getValue(),
        description: Description.create('Test building').getValue(),
        floorSize: FloorSize.create(100, 200).getValue(),
      };

      // Act
      const buildingResult = Building.create(buildingProps);

      // Assert
      expect(buildingResult.isSuccess).toBe(true);
      expect(buildingResult.getValue()).toBeInstanceOf(Building);
    });

    it.each([
      [
        Name.create('Building1').getValue(),
        null,
        Description.create('Test building').getValue(),
        FloorSize.create(100, 200).getValue(),
      ],
      [Name.create('Building1').getValue(), Code.create('A1').getValue(), null, FloorSize.create(100, 200).getValue()],
      [
        Name.create('Building1').getValue(),
        Code.create('A1').getValue(),
        Description.create('Test building').getValue(),
        null,
      ],
    ])('should fail if any required property is null or undefined', (name, code, description, floorSize) => {
      // Arrange
      const buildingProps = {
        name,
        code,
        description,
        floorSize,
      };

      // Act
      const buildingResult = Building.create(buildingProps);

      // Assert
      expect(buildingResult.isFailure).toBe(true);
    });
  });
});
