import { UniqueEntityID } from '../core/domain/UniqueEntityID';
import { Mapper } from '../core/infra/Mapper';
import { Result } from '../core/logic/Result';
import IRobotPersistence from '../dataschema/IRobotPersistence';
import { Code } from '../domain/Robot/ValueObjects/code';
import { Description } from '../domain/Robot/ValueObjects/description';
import { Nickname } from '../domain/Robot/ValueObjects/nickname';
import { SerialNumber } from '../domain/Robot/ValueObjects/serialNumber';
import { Robot } from '../domain/Robot/robot';
import { Name as RobotType } from '../domain/RobotType/ValueObjects/name';
import IRobotDTO from '../dto/IRobotDTO';

export class RobotMap extends Mapper<Robot> {
  public static toDTO(robot: Robot): IRobotDTO {
    return {
      id: robot.id.toString(),
      code: robot.code.value,
      description: robot.description.value,
      nickname: robot.nickname.value,
      serialNumber: robot.serialNumber.value,
      available: robot.available,
      type: robot.type.value,
    } as IRobotDTO;
  }

  public static toDomain(raw: any): Result<Robot> {
    const code = Code.create(raw.code as string);
    const description = Description.create(raw.description as string);
    const nickname = Nickname.create(raw.nickname as string);
    const serialNumber = SerialNumber.create(raw.serialNumber as string);
    const available = raw.available as boolean;
    const type = RobotType.create(raw.type as string);

    const combinedResults = Result.combine([code, description, nickname, serialNumber, type]);

    if (combinedResults.isFailure) {
      return Result.fail<Robot>(combinedResults.error);
    }

    const robotOrError = Robot.create(
      {
        code: code.getValue(),
        description: description.getValue(),
        nickname: nickname.getValue(),
        serialNumber: serialNumber.getValue(),
        available,
        type: type.getValue(),
      },
      new UniqueEntityID(raw.id),
    );

    robotOrError.isFailure ? console.log(robotOrError.error) : '';

    return robotOrError;
  }

  public static toPersistence(robot: Robot): IRobotPersistence {
    return {
      id: robot.id.toString(),
      code: robot.code.value,
      description: robot.description.value,
      nickname: robot.nickname.value,
      serialNumber: robot.serialNumber.value,
      available: robot.available,
      type: robot.type.value,
    };
  }
}
