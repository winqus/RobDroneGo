import { UniqueEntityID } from '../../core/domain/UniqueEntityID';
import { Code as BuildingCode } from '../../domain/Building/ValueObjects/code';
import { Passage } from '../../domain/Passage/passage';
import { PassageMap } from '../PassageMap';

describe('PassageMap', () => {
  describe('toDTO', () => {
    it('should map a Passage to IPassageDTO', () => {
      // Arrange
      const passageProps = {
        buildingCode1: BuildingCode.create('BA').getValue(),
        buildingCode2: BuildingCode.create('BB').getValue(),
        floorNumber1: 2,
        floorNumber2: 3,
      };
      const passage = Passage.create(passageProps, new UniqueEntityID()).getValue();

      // Act
      const passageDTO = PassageMap.toDTO(passage);

      // Assert
      expect(passageDTO).toBeDefined();
      expect(passageDTO.id).toEqual(passage.id.toString());
      expect(passageDTO.buildingCode1).toEqual(passageProps.buildingCode1.value);
      expect(passageDTO.buildingCode2).toEqual(passageProps.buildingCode2.value);
      expect(passageDTO.floorNumber1).toEqual(passageProps.floorNumber1);
      expect(passageDTO.floorNumber2).toEqual(passageProps.floorNumber2);
    });
  });

  describe('toDomain', () => {
    it('should map a raw object to a Passage domain entity', () => {
      // Arrange
      const rawPassage = {
        id: 'some-id',
        buildingCode1: 'BX',
        buildingCode2: 'BY',
        floorNumber1: 2,
        floorNumber2: 2,
      };

      // Act
      const passage = PassageMap.toDomain(rawPassage).getValue();

      // Assert
      expect(passage).toBeDefined();
      expect(passage.id.toString()).toEqual(rawPassage.id);
      expect(passage.buildingCode1.value).toEqual(rawPassage.buildingCode1);
      expect(passage.buildingCode2.value).toEqual(rawPassage.buildingCode2);
      expect(passage.floorNumber1).toEqual(rawPassage.floorNumber1);
      expect(passage.floorNumber2).toEqual(rawPassage.floorNumber2);
    });

    it('should fail to map a raw object to a Passage domain entity with invalid properties', () => {
      // Arrange
      const rawPassage = {
        id: 'some-id',
        buildingCode1: 'BX',
        buildingCode2: null,
        floorNumber1: 3,
        floorNumber2: 3,
      };

      // Act
      const passageResult = PassageMap.toDomain(rawPassage);

      // Assert
      expect(passageResult.isFailure).toBe(true);
      expect(passageResult.error).toMatch(/null/);
    });
  });

  describe('toPersistence', () => {
    it('should map a Passage to a raw object for persistence', () => {
      // Arrange
      const passageProps = {
        buildingCode1: BuildingCode.create('BA').getValue(),
        buildingCode2: BuildingCode.create('BB').getValue(),
        floorNumber1: 1,
        floorNumber2: 1,
      };
      const passage = Passage.create(passageProps, new UniqueEntityID('some-id')).getValue();

      // Act
      const rawPassage = PassageMap.toPersistence(passage);

      // Assert
      expect(rawPassage).toBeDefined();
      expect(rawPassage.id).toEqual('some-id');
      expect(rawPassage.buildingCode1).toEqual(passageProps.buildingCode1.value);
      expect(rawPassage.buildingCode2).toEqual(passageProps.buildingCode2.value);
      expect(rawPassage.floorNumber1).toEqual(passageProps.floorNumber1);
      expect(rawPassage.floorNumber2).toEqual(passageProps.floorNumber2);
    });
  });
});
