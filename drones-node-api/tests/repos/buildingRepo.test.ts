import { MockProxy, mock } from 'jest-mock-extended';
import { Document, HydratedDocument, Model } from 'mongoose';
import { Container } from 'typedi';
import { UniqueEntityID } from '../../src/core/domain/UniqueEntityID';
import { Building } from '../../src/domain/Building/building';
import BuildingRepo from '../../src/repos/buildingRepo';

describe('BuildingRepo', () => {
  let buildingRepo: BuildingRepo;
  let buildingSchemaMock: MockProxy<Model<Document>>;
  let buildingStub: Building;

  beforeEach(() => {
    buildingSchemaMock = mock<Model<any & Document>>();
    Container.set('buildingSchema', buildingSchemaMock);

    buildingRepo = new BuildingRepo(buildingSchemaMock as any);

    buildingStub = {
      id: new UniqueEntityID(),
      name: { value: 'Building1' },
      code: { value: 'A1' },
      description: { value: 'Test building' },
      floorSize: { value: { length: 100, width: 200 } },
    } as Building;
  });

  describe('exists', () => {
    it('should return true if building exists', async () => {
      buildingSchemaMock.findOne.mockResolvedValue({} as HydratedDocument<any, any, any>);

      const result = await buildingRepo.exists(buildingStub);

      expect(result).toBe(true);
    });

    it('should return false if building does not exist', async () => {
      buildingSchemaMock.findOne.mockResolvedValue(null as HydratedDocument<any, any, any>);

      const result = await buildingRepo.exists(buildingStub);

      expect(result).toBe(false);
    });
  });

  describe('save', () => {
    it('should save a new building successfully', async () => {
      buildingSchemaMock.findOne.mockResolvedValue(null as HydratedDocument<any, any, any>);
      buildingSchemaMock.create.mockResolvedValue({
        id: buildingStub.id.toString(),
        name: buildingStub.name.value,
        code: buildingStub.code.value,
        description: buildingStub.description.value,
        floorSizeLength: buildingStub.floorSize.value.length,
        floorSizeWidth: buildingStub.floorSize.value.width,
      } as HydratedDocument<any, any, any>);

      await buildingRepo.save(buildingStub);

      expect(buildingSchemaMock.create).toHaveBeenCalledTimes(1);
    });

    it('should update an existing building successfully', async () => {
      const saveStub = jest.fn();
      buildingSchemaMock.findOne.mockResolvedValue({ save: saveStub } as HydratedDocument<any, any, any>);

      await buildingRepo.save(buildingStub);

      expect(saveStub).toHaveBeenCalledTimes(1);
    });
  });

  describe('findById', () => {
    it('should return a building if it exists', async () => {
      buildingSchemaMock.findOne.mockResolvedValue({
        id: buildingStub.id.toString(),
        name: buildingStub.name.value,
        code: buildingStub.code.value,
        description: buildingStub.description.value,
        floorSizeLength: buildingStub.floorSize.value.length,
        floorSizeWidth: buildingStub.floorSize.value.width,
      } as HydratedDocument<any, any, any>);

      const result = await buildingRepo.findById(buildingStub.id.toString());

      expect(result.id).toEqual(buildingStub.id);
      expect(result.name.value).toEqual(buildingStub.name.value);
      expect(result.code.value).toEqual(buildingStub.code.value);
      expect(result.description.value).toEqual(buildingStub.description.value);
      expect(result.floorSize.value.length).toEqual(buildingStub.floorSize.value.length);
      expect(result.floorSize.value.width).toEqual(buildingStub.floorSize.value.width);
    });

    it('should return null if building does not exist', async () => {
      buildingSchemaMock.findOne.mockResolvedValue(null as HydratedDocument<any, any, any>);

      const result = await buildingRepo.findById(buildingStub.id.toString());

      expect(result).toBeNull();
    });
  });

  describe('findElevatorsInBuilding', () => {
    it('should return elevator data if building with elevators exists', async () => {
      const buildingCode = 'sampleBuildingCode';

      buildingSchemaMock.findOne.mockResolvedValue({
        code: buildingStub.code.value,
        elevator: {
          number: 1,
          make: 'Sample Make',
          model: 'Sample Model',
          serialNumber: '12345',
          description: 'Sample Elevator',
        },
      } as HydratedDocument<any, any, any>);

      const elevatorData = await buildingRepo.findElevatorsInBuilding(buildingCode);

      expect(elevatorData.length).toEqual(1);
    });
  });

  it('should return empty array if building does not exist', async () => {
    const buildingCode = 'nonExistentBuildingCode';

    buildingSchemaMock.findOne.mockResolvedValue(null as HydratedDocument<any, any, any>);

    const elevatorData = await buildingRepo.findElevatorsInBuilding(buildingCode);

    expect(elevatorData.length).toBe(0);
  });
});
