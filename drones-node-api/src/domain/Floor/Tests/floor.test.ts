import { Code as BuildingCode } from '../../Building/ValueObjects/code';
import { Description } from '../../Building/ValueObjects/description';
import { Floor } from '../floor';

describe('Floor', () => {
  it('should create a valid floor', () => {
    const validFloorProps = {
      floorNumber: 123,
      description: Description.create('Sample floor description').getValue(),
      servedByElevator: true,
      buildingCode: BuildingCode.create('ABC1').getValue(),
    };

    const floorResult = Floor.create(validFloorProps);
    expect(floorResult.isSuccess).toBe(true);
    const floor = floorResult.getValue();
    expect(floor.floorNumber).toBe(123);
    expect(floor.description.value).toBe('Sample floor description');
    expect(floor.servedByElevator).toBe(true);
    expect(floor.buildingCode.value).toBe('ABC1');
  });

  it('should create a valid floor even without description', () => {
    const validFloorProps = {
      floorNumber: 123,
      description: Description.create(null).getValue(),
      servedByElevator: true,
      buildingCode: BuildingCode.create('ABC1').getValue(),
    };

    const floorResult = Floor.create(validFloorProps);
    expect(floorResult.isSuccess).toBe(true);
    const floor = floorResult.getValue();
    expect(floor.floorNumber).toBe(123);
    expect(floor.description.value).toBe('');
    expect(floor.servedByElevator).toBe(true);
    expect(floor.buildingCode.value).toBe('ABC1');
  });

  it('should fail to create a floor with code null', () => {
    const invalidFloorProps = {
      floorNumber: null,
      description: Description.create('Sample floor description').getValue(),
      servedByElevator: false,
      buildingCode: BuildingCode.create('ABC1').getValue(),
    };

    const floorResult = Floor.create(invalidFloorProps);
    expect(floorResult.isFailure).toBe(true);
    expect(floorResult.error).toBe('floorNumber is null or undefined');
  });
});
