import { Result } from '../../core/logic/Result';
import IBuildingDTO from '../../dto/IBuildingDTO';
import IElevatorDTO from '../../dto/IElevatorDTO';

export default interface IElevatorService {
  createElevator(elevatorDTO: IElevatorDTO, buildingCode: string, floors: number[]): Promise<Result<IBuildingDTO>>;
}
