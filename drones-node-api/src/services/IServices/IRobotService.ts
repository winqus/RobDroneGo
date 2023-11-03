import { Result } from '../../core/logic/Result';
import IRobotDTO from '../../dto/IRobotDTO';
import IRobotTypeDTO from '../../dto/IRobotTypeDTO';

export default interface IRobotService {
  createRobot(robotDTO: IRobotDTO): Promise<Result<IRobotDTO>>;

  changeRobotState(updatedRobotDTO: Partial<IRobotDTO>): Promise<Result<IRobotDTO>>;

  getAllRobots(): Promise<Result<IRobotDTO[]>>;

  findRobotByType(robotType: Partial<IRobotTypeDTO>): Promise<Result<IRobotDTO[]>>;
}
