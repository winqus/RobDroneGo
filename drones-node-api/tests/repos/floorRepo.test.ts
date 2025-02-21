import { Document, HydratedDocument, Model } from 'mongoose';
import { Container } from 'typedi';
import { UniqueEntityID } from '../../src/core/domain/UniqueEntityID';
import { Floor } from '../../src/domain/Floor/floor';
import FloorRepo from '../../src/repos/floorRepo';

describe('FloorRepo', () => {
  let floorRepo: FloorRepo;
  let floorSchemaMock: jest.Mocked<Model<Document>>;
  let floorStub: Floor;

  beforeEach(() => {
    floorSchemaMock = ({
      findOne: jest.fn(),
      create: jest.fn(),
    } as unknown) as jest.Mocked<Model<Document>>;
    Container.set('floorSchema', floorSchemaMock);

    floorRepo = new FloorRepo(floorSchemaMock as any, null as any);

    floorStub = {
      id: new UniqueEntityID(),
      floorNumber: 13,
      description: { value: 'Sample floor description' },
      servedByElevator: true,
      buildingCode: { value: 'A11' },
    } as Floor;
  });

  describe('exists', () => {
    it('should return true if floor exists', async () => {
      floorSchemaMock.findOne.mockResolvedValue({} as HydratedDocument<any, any, any>);

      const result = await floorRepo.exists(floorStub);

      expect(result).toBe(true);
    });

    it('should return false if floor does not exist', async () => {
      floorSchemaMock.findOne.mockResolvedValue(null as HydratedDocument<any, any, any>);

      const result = await floorRepo.exists(floorStub);

      expect(result).toBe(false);
    });
  });

  describe('save', () => {
    it('should save a new floor successfully', async () => {
      floorSchemaMock.findOne.mockResolvedValue(null as HydratedDocument<any, any, any>);
      floorSchemaMock.create.mockResolvedValue({
        id: floorStub.id.toString(),
        floorNumber: floorStub.floorNumber,
        description: floorStub.description.value,
        servedByElevator: floorStub.servedByElevator,
        buildingCode: floorStub.buildingCode.value,
      } as HydratedDocument<any, any, any>);

      await floorRepo.save(floorStub);

      expect(floorSchemaMock.create).toHaveBeenCalledTimes(1);
    });
  });

  describe('update', () => {
    it('should update an existing floor successfully', async () => {
      const saveStub = jest.fn();
      floorSchemaMock.findOne.mockResolvedValue({ save: saveStub } as HydratedDocument<any, any, any>);

      await floorRepo.save(floorStub);

      expect(saveStub).toHaveBeenCalledTimes(1);
    });

    it('should handle errors and throw them', async () => {
      floorSchemaMock.findOne.mockRejectedValue(new Error('Database error'));

      await expect(floorRepo.save(floorStub)).rejects.toThrow('Database error');
    });
  });
});
