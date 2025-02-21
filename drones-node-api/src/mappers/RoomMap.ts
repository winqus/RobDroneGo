import { UniqueEntityID } from '../core/domain/UniqueEntityID';
import { Mapper } from '../core/infra/Mapper';
import { Result } from '../core/logic/Result';
import { IRoomPersistence } from '../dataschema/IRoomPersistence';
import { Description } from '../domain/Building/ValueObjects/description';
import { FloorSize as Size } from '../domain/Building/ValueObjects/floorSize';
import { RoomCategory } from '../domain/Room/ValueObjects/category';
import { Name } from '../domain/Room/ValueObjects/name';
import { Room } from '../domain/Room/room';
import { PositionOnMap as Position } from '../domain/common/positionOnMap';
import IRoomDTO from '../dto/IRoomDTO';

export class RoomMap extends Mapper<Room> {
  public static toDTO(passage: Room): IRoomDTO {
    return {
      id: passage.id.toString(),
      name: passage.name.value,
      description: passage.description?.value,
      size: passage.size.value,
      position: passage.position.value,
      category: passage.category.valueOf() as string,
      floorId: passage.floorId.toString(),
    } as IRoomDTO;
  }

  public static toDomain(raw: any): Result<Room> {
    const name = Name.create(raw?.name as string);
    const description = Description.create(raw?.description as string);
    const size = Size.create(raw?.size.length as number, raw?.size.width as number);
    const position = Position.create(raw?.position.x as number, raw?.position.y as number);
    const category = (raw?.category as string) as RoomCategory;
    const floorId = new UniqueEntityID(raw?.floorId as string);

    const combinedResults = Result.combine([name, description, size, position]);
    if (combinedResults.isFailure) {
      return Result.fail<Room>(combinedResults.error);
    }

    const passageOrError = Room.create(
      {
        name: name.getValue(),
        description: description.getValue(),
        size: size.getValue(),
        position: position.getValue(),
        category: category,
        floorId: floorId,
      },
      new UniqueEntityID(raw.id),
    );

    passageOrError.isFailure ? console.log(passageOrError.error) : '';

    return passageOrError;
  }

  public static toPersistence(room: Room): IRoomPersistence {
    return {
      id: room.id.toString(),
      name: room.name.value,
      description: room.description?.value,
      size: room.size.value,
      position: room.position.value,
      category: room.category.valueOf(),
      floorId: room.floorId.toString(),
    };
  }
}
