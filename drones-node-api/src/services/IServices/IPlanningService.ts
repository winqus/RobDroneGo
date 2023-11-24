import { ParsedQs } from 'qs';
import { Result } from '../../core/logic/Result';

export default interface IPlanningService {
  calculateCells(query: ParsedQs): Promise<Result<any>>;
}
