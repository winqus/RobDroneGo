import { Inject, Service } from 'typedi';
import config from '../../config';
import { Result } from '../core/logic/Result';
import { Robot } from '../domain/Robot/robot';
import IRobotDTO from '../dto/IRobotDTO';
import { RobotMap } from '../mappers/RobotMap';
import IRobotRepo from './IRepos/IRobotRepo';
import IRobotTypeRepo from './IRepos/IRobotTypeRepo';
import IRobotService from './IServices/IRobotService';

@Service()
export default class RobotService implements IRobotService {
  constructor(
    @Inject(config.repos.robot.name) private robotRepo: IRobotRepo,
    @Inject(config.repos.robotType.name) private robotTypeRepo: IRobotTypeRepo,
  ) {}

  public async createRobot(robotDTO: IRobotDTO): Promise<Result<IRobotDTO>> {
    try {
      const robotOrError = RobotMap.toDomain(robotDTO);

      if (robotOrError.isFailure) {
        return Result.fail<IRobotDTO>(robotOrError.errorValue().toString());
      }

      const robotResult = robotOrError.getValue();
      robotResult.available = true;
      const robotTypeResult = await this.robotTypeRepo.findByName(robotResult.type.value);

      if (!robotTypeResult) {
        return Result.fail<IRobotDTO>('Robot Type does not exist.');
      }

      await this.robotRepo.save(robotResult);

      const robotDTOResult = RobotMap.toDTO(robotResult) as IRobotDTO;

      return Result.ok<IRobotDTO>(robotDTOResult);
    } catch (error) {
      throw error;
    }
  }

  public async changeRobotState(updatedRobotDTO: Partial<IRobotDTO>): Promise<Result<IRobotDTO>> {
    try {
      const existingRobot = await this.robotRepo.findByCode(updatedRobotDTO.code);
      if (!existingRobot) {
        return Result.fail<IRobotDTO>('Robot does not exist.');
      }

      if (existingRobot.available === updatedRobotDTO.available) {
        return Result.fail<IRobotDTO>('Robot is already in the requested state.');
      }

      const updatedRobotData = {
        id: existingRobot.id.toString(),
        code: existingRobot.code.value,
        description: existingRobot.description.value,
        nickname: existingRobot.nickname.value,
        serialNumber: existingRobot.serialNumber.value,
        available: updatedRobotDTO.available,
        type: existingRobot.type.value,
      };

      const robotOrError = RobotMap.toDomain(updatedRobotData);

      if (robotOrError.isFailure) {
        return Result.fail<IRobotDTO>(robotOrError.errorValue().toString());
      }

      await this.robotRepo.save(robotOrError.getValue());

      const robotDTOResult = RobotMap.toDTO(robotOrError.getValue()) as IRobotDTO;

      return Result.ok<IRobotDTO>(robotDTOResult);
    } catch (error) {
      throw error;
    }
  }
}
