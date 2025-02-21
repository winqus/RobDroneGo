import { UniqueEntityID } from '../../../core/domain/UniqueEntityID';
import { Description } from '../../Building/ValueObjects/description';
import { FloorSize as Size } from '../../Building/ValueObjects/floorSize';
import { PositionOnMap as Position } from '../../common/positionOnMap';
import { RoomCategory } from '../ValueObjects/category';
import { Name } from '../ValueObjects/name';
import { Room } from '../room';

describe('Room', () => {
  it('should successfully create a Room instance with valid properties', () => {
    const props = {
      name: Name.create('Room1').getValue(),
      description: Description.create('Sample Room').getValue(),
      category: RoomCategory.Office,
      size: Size.create(100, 120).getValue(),
      position: Position.create(1, 2).getValue(),
      floorId: new UniqueEntityID(),
    };

    const roomResult = Room.create(props);
    expect(roomResult.isSuccess).toBe(true);
    expect(roomResult.getValue()).toBeInstanceOf(Room);
  });

  it('should fail for null or undefined mandatory properties', () => {
    const props = {
      name: null,
      description: null,
      category: null,
      size: null,
      position: null,
      floorId: null,
    };

    const roomResult = Room.create(props as any);
    expect(roomResult.isFailure).toBe(true);
  });
});
