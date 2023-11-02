import { Repo } from '../../core/infra/Repo';
import { Robot } from '../../domain/Robot/robot';

export default interface IFloorRepo extends Repo<Robot> {
  save(robot: Robot): Promise<Robot>;
}
