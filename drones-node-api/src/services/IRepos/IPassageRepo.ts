import { Repo } from '../../core/infra/Repo';
import { Passage } from '../../domain/Passage/passage';

export default interface IPassageRepo extends Repo<Passage> {
  save(passage: Passage): Promise<Passage>;

  findByCodes(passage: Passage): Promise<Passage>;
}
