import { Name as RobotType } from '../../RobotType/ValueObjects/name';
import { Code } from '../ValueObjects/code';
import { Description } from '../ValueObjects/description';
import { Nickname } from '../ValueObjects/nickname';
import { SerialNumber } from '../ValueObjects/serialNumber';
import { Robot, RobotProps } from '../robot';

describe('Robot', () => {
  describe('create', () => {
    it('should successfully create a Robot instance with valid properties', () => {
      // Arrange
      const robotProps: RobotProps = {
        code: Code.create('A1').getValue(),
        description: Description.create('Test robot').getValue(),
        nickname: Nickname.create('Test').getValue(),
        serialNumber: SerialNumber.create('1234567890').getValue(),
        available: true,
        type: RobotType.create('DRONE').getValue(),
        position: {
          floorNumber: 2,
          buildingCode: 'A',
          cellPosition: [8, 8],
        },
      };

      // Act
      const robotResult = Robot.create(robotProps);

      // Assert
      expect(robotResult.isSuccess).toBe(true);
      expect(robotResult.getValue()).toBeInstanceOf(Robot);
    });

    it.each([
      [
        Code.create('A1').getValue(),
        Description.create('Test robot').getValue(),
        null,
        SerialNumber.create('1234567890').getValue(),
        true,
        RobotType.create('DRONE').getValue(),
        {
          floorNumber: 1,
          buildingCode: 'A1',
          cellPosition: [0, 2],
        },
      ],
      [
        Code.create('A1').getValue(),
        Description.create('Test robot').getValue(),
        Nickname.create('Test').getValue(),
        null,
        true,
        RobotType.create('DRONE').getValue(),
        {
          floorNumber: 1,
          buildingCode: 'A1',
          cellPosition: [0, 1],
        },
      ],
      [
        Code.create('A1').getValue(),
        Description.create('Test robot').getValue(),
        Nickname.create('Test').getValue(),
        SerialNumber.create('1234567890').getValue(),
        true,
        null,
        {
          floorNumber: 1,
          buildingCode: 'A1',
          cellPosition: [1, 0],
        },
      ],
    ])(
      'should fail if any required property is null or undefined',
      (code, description, nickname, serialNumber, available, type, position) => {
        // Arrange
        const robotProps = {
          code,
          description,
          nickname,
          serialNumber,
          available,
          type,
          position: position,
        };

        // Act
        const robotResult = Robot.create(robotProps);

        // Assert
        expect(robotResult.isFailure).toBe(true);
      },
    );
  });
});
