import { Repo } from '../../core/infra/Repo';
import { Robot } from '../../domain/Robot/robot';

export default interface IFloorRepo extends Repo<Robot> {
  save(robot: Robot): Promise<Robot>;

  findById(robotId: string): Promise<Robot | null>;

  findByCode(robotCode: string): Promise<Robot | null>;

  findAll(): Promise<Robot[]>;

  findByType(robotTypeName: string): Promise<Robot[]>;
}
