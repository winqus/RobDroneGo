import { Result } from '../../core/logic/Result';
import IRobotDTO from '../../dto/IRobotDTO';

export default interface IRobotService {
  createRobot(robotDTO: IRobotDTO): Promise<Result<IRobotDTO>>;
  changeRobotState(updatedRobotDTO: Partial<IRobotDTO>): Promise<Result<IRobotDTO>>;
}
