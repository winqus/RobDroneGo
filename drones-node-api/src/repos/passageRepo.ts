import { Document, FilterQuery, Model } from 'mongoose';
import { Inject, Service } from 'typedi';
import { UniqueEntityID } from '../core/domain/UniqueEntityID';
import { IPassagePersistence } from '../dataschema/IPassagePersistence';
import { Passage } from '../domain/Passage/passage';
import { PassageMap } from '../mappers/PassageMap';
import IPassageRepo from '../services/IRepos/IPassageRepo';

@Service()
export default class PassageRepo implements IPassageRepo {
  constructor(@Inject('passageSchema') private passageSchema: Model<IPassagePersistence & Document>) {}

  public async exists(passage: Passage): Promise<boolean> {
    const idX = passage.id instanceof UniqueEntityID ? passage.id.toValue() : passage.id;

    const query = { domainId: idX };
    const passageDocument = await this.passageSchema.findOne(query as FilterQuery<IPassagePersistence & Document>);

    return Boolean(passageDocument) === true;
  }

  public async findByCodes(passage: Passage): Promise<Passage> {
    const query = {
      buildingCode1: passage.buildingCode1.value,
      buildingCode2: passage.buildingCode2.value,
      floorNumber1: passage.floorNumber1,
      floorNumber2: passage.floorNumber2,
    };

    const passageRecord = await this.passageSchema.findOne(query as FilterQuery<IPassagePersistence & Document>);
    const passageResult = PassageMap.toDomain(passageRecord);
    if (passageResult.isSuccess) {
      return passageResult.getValue();
    } else {
      return null;
    }
  }

  public async findAllByBuildingCodes(buildingCode1: string, buildingCode2: string): Promise<Passage[]> {
    const query = {
      $or: [
        { buildingCode1: buildingCode1, buildingCode2: buildingCode2 },
        { buildingCode1: buildingCode2, buildingCode2: buildingCode1 },
      ],
    };

    const passages = await this.passageSchema.find(query as FilterQuery<IPassagePersistence & Document>);

    return passages.map((passage) => PassageMap.toDomain(passage).getValue());
  }

  public async getAll(): Promise<Passage[]> {
    const passages = await this.passageSchema.find({});

    return passages.map((passage) => PassageMap.toDomain(passage).getValue());
  }

  public async save(passage: Passage): Promise<Passage> {
    const query = {
      buildingCode1: passage.buildingCode1.value,
      buildingCode2: passage.buildingCode2.value,
      floorNumber1: passage.floorNumber1,
      floorNumber2: passage.floorNumber2,
    };

    const passageDocument = await this.passageSchema.findOne(query);

    try {
      if (passageDocument === null) {
        const rawPassage: any = PassageMap.toPersistence(passage);
        const passageCreated = await this.passageSchema.create(rawPassage);

        return PassageMap.toDomain(passageCreated)?.getValue() ?? null;
      } else {
        passageDocument.buildingCode1 = passage.buildingCode1.value;
        passageDocument.buildingCode2 = passage.buildingCode2.value;
        passageDocument.floorNumber1 = passage.floorNumber1;
        passageDocument.floorNumber2 = passage.floorNumber2;

        await passageDocument.save();

        return passage;
      }
    } catch (error) {
      throw error;
    }
  }
}
