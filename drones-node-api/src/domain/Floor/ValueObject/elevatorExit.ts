import e from 'express';
import { ValueObject } from '../../../core/domain/ValueObject';

import { Guard } from '../../../core/logic/Guard';
import { Result } from '../../../core/logic/Result';

export interface elevatorExitProps {
  cellPosition: [number, number];
}

export class ElevatorExit extends ValueObject<elevatorExitProps> {
  get cellPosition(): [number, number] {
    return this.props.cellPosition;
  }

  private constructor(props: elevatorExitProps) {
    super(props);
  }

  public static create(cellPosition: [number, number]): Result<ElevatorExit> {
    const guardResult = Guard.combine([
      Guard.isInteger(cellPosition[0], 'cellPosition[0]'),
      Guard.isInteger(cellPosition[1], 'cellPosition[1]'),
      Guard.isTrue(cellPosition[0] >= 0, 'cellPosition[0] must be greater than or equal to 0'),
      Guard.isTrue(cellPosition[1] >= 0, 'cellPosition[1] must be greater than or equal to 0'),
    ]);

    if (!guardResult.succeeded) {
      return Result.fail<ElevatorExit>(guardResult.message);
    } else {
      return Result.ok<ElevatorExit>(new ElevatorExit({ cellPosition }));
    }
  }
}
