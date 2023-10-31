import { Repo } from '../../core/infra/Repo';
import { RobotType } from '../../domain/RobotType/robotType';

export default interface IRobotTypeRepo extends Repo<RobotType> {
  save(robotType: RobotType): Promise<RobotType>;

  findById(robotTypeId: string): Promise<RobotType>;

  findByName(robotTypeName: string): Promise<RobotType>;

  findByBrandAndModel(robotTypeBrand: string, robotTypeModel: string): Promise<RobotType>;
}
