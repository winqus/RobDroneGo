import { UniqueEntityID } from '../core/domain/UniqueEntityID';
import { Description } from '../domain/Building/ValueObjects/description';
import { FloorSize as Size } from '../domain/Building/ValueObjects/floorSize';
import { RoomCategory } from '../domain/Room/ValueObjects/category';
import { Name } from '../domain/Room/ValueObjects/name';
import { Room } from '../domain/Room/room';
import { PositionOnMap as Position } from '../domain/common/positionOnMap';
import IRoomDTO from '../dto/IRoomDTO';
import { RoomMap } from './RoomMap';

describe('RoomMap', () => {
  const roomProps = {
    name: Name.create('Room1').getValue(),
    description: Description.create('Sample Room').getValue(),
    category: RoomCategory.Office,
    size: Size.create(100, 120).getValue(),
    position: Position.create(1, 2).getValue(),
    floorId: new UniqueEntityID(),
  };

  const room = Room.create(roomProps).getValue();

  it('should map to DTO correctly', () => {
    const roomDTO = RoomMap.toDTO(room);
    expect(roomDTO).toMatchObject({
      id: room.id.toString(),
      name: room.name.value,
      description: room.description?.value,
      size: { width: room.size.value.width, length: room.size.value.length },
      position: { x: room.position.value.x, y: room.position.value.y },
      category: room.category.valueOf(),
      floorId: room.floorId.toString(),
    });
  });

  it('should map to Domain correctly', () => {
    const raw = RoomMap.toPersistence(room);
    const roomDomainResult = RoomMap.toDomain(raw);
    expect(roomDomainResult.isSuccess).toBe(true);
  });

  it('should map to Persistence correctly', () => {
    const persistence = RoomMap.toPersistence(room);
    expect(persistence).toMatchObject({
      id: room.id.toString(),
      name: room.name.value,
      description: room.description?.value,
      size: room.size.value,
      position: room.position.value,
      category: room.category.valueOf(),
      floorId: room.floorId.toString(),
    });
  });
});
