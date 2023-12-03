import { ParsedQs } from 'qs';
import { Inject, Service } from 'typedi';
import config from '../../config';
import { Result } from '../core/logic/Result';
import IFloorRepo from './IRepos/IFloorRepo';
import IRoomRepo from './IRepos/IRoomRepo';
import IPlanningService from './IServices/IPlanningService';

@Service()
export default class PlanningService implements IPlanningService {
  constructor(
    @Inject(config.repos.floor.name) private floorRepo: IFloorRepo,
    @Inject(config.repos.room.name) private roomRepo: IRoomRepo,
  ) {}

  public async calculateCells(query: ParsedQs): Promise<Result<any>> {
    const originRoom = await this.roomRepo.findById(query.origin_room as string);
    const destinationRoom = await this.roomRepo.findById(query.destination_room as string);

    if (!originRoom) {
      return Result.fail<any>('Origin room not found');
    }

    if (!destinationRoom) {
      return Result.fail<any>('Destination room not found');
    }

    const originFloor = await this.floorRepo.getFloorOfRoom(originRoom.name.value as string);
    const destinationFloor = await this.floorRepo.getFloorOfRoom(destinationRoom.name.value as string);

    const originRoomPositionX = originRoom.position.props.x;
    const originRoomPositionY = originRoom.position.props.y;
    const originRoomWidth = originRoom.size.props.width;
    const originRoomLength = originRoom.size.props.length;

    const destinationRoomPositionX = destinationRoom.position.props.x;
    const destinationRoomPositionY = destinationRoom.position.props.y;
    const destinationRoomWidth = destinationRoom.size.props.width;
    const destinationRoomLength = destinationRoom.size.props.length;

    let origin_map_cell_x = null;
    let origin_map_cell_y = null;
    let destination_map_cell_x = null;
    let destination_map_cell_y = null;

    for (let y = originRoomPositionY; y <= originRoomPositionY + originRoomLength; y++) {
      for (let x = originRoomPositionX; x <= originRoomPositionX + originRoomWidth; x++) {
        if (originFloor.map.map[y][x] === 4 || originFloor.map.map[y][x] === 5) {
          origin_map_cell_x = x;
          origin_map_cell_y = y;
        }
      }
    }

    for (let y = destinationRoomPositionY; y <= destinationRoomPositionY + destinationRoomLength; y++) {
      for (let x = destinationRoomPositionX; x <= destinationRoomPositionX + destinationRoomWidth; x++) {
        if (destinationFloor.map.map[y][x] === 4 || destinationFloor.map.map[y][x] === 5) {
          destination_map_cell_x = x;
          destination_map_cell_y = y;
        }
      }
    }

    if (
      origin_map_cell_x !== null &&
      origin_map_cell_y !== null &&
      destination_map_cell_x !== null &&
      destination_map_cell_y !== null
    ) {
      const result = {
        origin_map_cell_x: origin_map_cell_x,
        origin_map_cell_y: origin_map_cell_y,
        destination_map_cell_x: destination_map_cell_x,
        destination_map_cell_y: destination_map_cell_y,
      };

      return Result.ok<any>(result);
    } else {
      return Result.fail<any>('No room door found');
    }
  }
}
