import { ValueObject } from '../../../core/domain/ValueObject';

import { Guard } from '../../../core/logic/Guard';
import { Result } from '../../../core/logic/Result';

interface PassageExitProps {
  cellPosition: [number, number];
  destination: { buildingCode: string; floorNumber: number };
}

export class PassageExit extends ValueObject<PassageExitProps> {
  get cellPosition(): [number, number] {
    return this.props.cellPosition;
  }

  get destination(): { buildingCode: string; floorNumber: number } {
    return this.props.destination;
  }

  private constructor(props: PassageExitProps) {
    super(props);
  }

  public static create(
    cellPosition: [number, number],
    destination: { buildingCode: string; floorNumber: number },
  ): Result<PassageExit> {
    const guardResult = Guard.combine([
      Guard.isInteger(cellPosition[0], 'cellPosition[0]'),
      Guard.isInteger(cellPosition[1], 'cellPosition[1]'),
      Guard.isTrue(cellPosition[0] >= 0, 'cellPosition[0] must be greater than or equal to 0'),
      Guard.isTrue(cellPosition[1] >= 0, 'cellPosition[1] must be greater than or equal to 0'),
      Guard.isInteger(destination.floorNumber, 'destination.floorNumber'),
      Guard.isTrue(destination.floorNumber > 0, 'destination.floorNumber must be greater than 0'),
    ]);

    if (!guardResult.succeeded) {
      return Result.fail<PassageExit>(guardResult.message);
    } else {
      return Result.ok<PassageExit>(new PassageExit({ cellPosition, destination }));
    }
  }
}
