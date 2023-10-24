import { Document, HydratedDocument, Model } from 'mongoose';
import { Container } from 'typedi';
import { UniqueEntityID } from '../../src/core/domain/UniqueEntityID';
import { Building } from '../../src/domain/Building/building';
import BuildingRepo from '../../src/repos/buildingRepo';

describe('BuildingRepo', () => {
  let buildingRepo: BuildingRepo;
  let buildingSchemaMock: jest.Mocked<Model<Document>>;
  let buildingStub: Building;

  beforeEach(() => {
    buildingSchemaMock = ({
      findOne: jest.fn(),
      create: jest.fn(),
    } as unknown) as jest.Mocked<Model<Document>>;
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
});
