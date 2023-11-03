import { Repo } from '../../core/infra/Repo';
import { Passage } from '../../domain/Passage/passage';

export default interface IPassageRepo extends Repo<Passage> {
  save(passage: Passage): Promise<Passage>;

  findByCodes(passage: Passage): Promise<Passage>;

  findAllByBuildingCodes(buildingCode1: string, buildingCode2: string): Promise<Passage[]>;

  getAll(): Promise<Passage[]>;
}
