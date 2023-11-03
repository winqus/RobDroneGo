import { MockProxy, mock } from 'jest-mock-extended';
import { UniqueEntityID } from '../../src/core/domain/UniqueEntityID';
import { Description } from '../../src/domain/Building/Entities/ValueObjects/description';
import { IDNumber } from '../../src/domain/Building/Entities/ValueObjects/idNumber';
import { MakeModel } from '../../src/domain/Building/Entities/ValueObjects/makeModel';
import { SerialNumber } from '../../src/domain/Building/Entities/ValueObjects/serialNumber';
import { Elevator } from '../../src/domain/Building/Entities/elevator';
import { Building } from '../../src/domain/Building/building';
import { Floor } from '../../src/domain/Floor/floor';
import { BuildingMap } from '../../src/mappers/BuildingMap';
import { ElevatorMap } from '../../src/mappers/ElevatorMap';
import { FloorMap } from '../../src/mappers/FloorMap';
import IBuildingRepo from '../../src/services/IRepos/IBuildingRepo';
import IFloorRepo from '../../src/services/IRepos/IFloorRepo';
import ElevatorService from '../../src/services/elevatorService'; // Import the actual service

describe('ElevatorService', () => {
  let elevatorService: ElevatorService;
  let buildingRepoMock: MockProxy<IBuildingRepo>;
  let floorRepoMock: MockProxy<IFloorRepo>;

  const elevatorId = new UniqueEntityID();
  const elevatorDTO = {
    id: elevatorId.toString(),
    number: 1,
    make: 'Make',
    model: 'Model',
    serialNumber: 'SN123',
    description: 'Elevator Description',
  };
  const buildingId = new UniqueEntityID();
  const buildingCode = 'B';
  const floors = [1];
  let building = BuildingMap.toDomain({
    id: buildingId,
    code: buildingCode,
    name: 'Building',
    address: 'Address',
    floorSizeLength: 1,
    floorSizeWidth: 1,
    elevator: null,
  });

  beforeEach(() => {
    buildingRepoMock = mock<IBuildingRepo>();
    floorRepoMock = mock<IFloorRepo>();
    elevatorService = new ElevatorService(buildingRepoMock, floorRepoMock);

    building = BuildingMap.toDomain({
      id: buildingId,
      code: buildingCode,
      name: 'Building',
      address: 'Address',
      floorSizeLength: 1,
      floorSizeWidth: 1,
      elevator: null,
    });
  });

  describe('createElevator', () => {
    it('should successfully create an elevator and update building and floors', async () => {
      buildingRepoMock.findByCode.mockResolvedValue(Promise.resolve(building));
      buildingRepoMock.save.mockResolvedValue(Promise.resolve(building));
      floorRepoMock.findByCodes.mockResolvedValue(
        Promise.resolve([
          FloorMap.toDomain({
            id: new UniqueEntityID().toString(),
            floorNumber: 1,
            description: 'description',
            servedByElevator: false,
            buildingCode: buildingCode,
          }).getValue(),
        ]),
      );

      const result = await elevatorService.createElevator(elevatorDTO, buildingCode, floors);

      expect(result.isSuccess).toBe(true);
      expect(buildingRepoMock.findByCode).toHaveBeenCalledWith(buildingCode);
      expect(buildingRepoMock.save).toHaveBeenCalledWith(building);
      expect(floorRepoMock.findByCodes).toHaveBeenCalledWith(buildingCode, floors);
      expect(floorRepoMock.save).toHaveBeenCalledTimes(1);
    });

    it('should fail when the building does not exist', async () => {
      buildingRepoMock.findByCode.mockResolvedValue(Promise.resolve(null as any));

      const result = await elevatorService.createElevator(elevatorDTO, buildingCode, floors);

      expect(result.isFailure).toBe(true);
      expect(result.error).toBe('Building not found');
    });

    it('should fail when the building already has an elevator', async () => {
      building.elevator = ElevatorMap.toDomain(elevatorDTO);
      buildingRepoMock.findByCode.mockResolvedValue(Promise.resolve(building));

      const result = await elevatorService.createElevator(elevatorDTO, buildingCode, floors);

      expect(result.isFailure).toBe(true);
      expect(result.error).toBe('Building already has an elevator');
    });

    it('should fail when input data is invalid', async () => {
      const result = await elevatorService.createElevator(elevatorDTO, buildingCode, floors);

      expect(result.isFailure).toBe(true);
    });
  });
});
