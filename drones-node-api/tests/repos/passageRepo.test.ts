import { Document, Model } from 'mongoose';
import { Container } from 'typedi';
import { UniqueEntityID } from '../../src/core/domain/UniqueEntityID';
import { Passage } from '../../src/domain/Passage/passage';
import PassageRepo from '../../src/repos/passageRepo';

describe('PassageRepo', () => {
  let passageRepo: PassageRepo;
  let passageSchemaMock: jest.Mocked<Model<Document>>;
  let passageStub: Passage;

  beforeEach(() => {
    passageSchemaMock = ({
      findOne: jest.fn(),
      create: jest.fn(),
    } as unknown) as jest.Mocked<Model<Document>>;
    Container.set('passageSchema', passageSchemaMock);

    passageRepo = new PassageRepo(passageSchemaMock as any);

    passageStub = {
      id: new UniqueEntityID(),
      buildingCode1: { value: 'BA' },
      buildingCode2: { value: 'BB' },
      floorNumber1: 2,
      floorNumber2: 2,
    } as Passage;
  });

  describe('exists', () => {
    it('should return true if passage exists', async () => {
      passageSchemaMock.findOne.mockResolvedValue({} as any);

      const result = await passageRepo.exists(passageStub);

      expect(result).toBe(true);
    });

    it('should return false if passage does not exist', async () => {
      passageSchemaMock.findOne.mockResolvedValue(null as any);

      const result = await passageRepo.exists(passageStub);

      expect(result).toBe(false);
    });
  });

  describe('save', () => {
    it('should save a new passage successfully', async () => {
      passageSchemaMock.findOne.mockResolvedValue(null as any);
      passageSchemaMock.create.mockResolvedValue({
        id: passageStub.id.toString(),
        buildingCode1: passageStub.buildingCode1.value,
        buildingCode2: passageStub.buildingCode2.value,
        floorNumber1: passageStub.floorNumber1,
        floorNumber2: passageStub.floorNumber2,
      } as any);

      await passageRepo.save(passageStub);

      expect(passageSchemaMock.create).toHaveBeenCalledTimes(1);
    });
  });

  it('should update an existing passage successfully', async () => {
    const saveStub = jest.fn();
    passageSchemaMock.findOne.mockResolvedValue({ save: saveStub } as any);

    await passageRepo.save(passageStub);

    expect(saveStub).toHaveBeenCalledTimes(1);
  });
});
