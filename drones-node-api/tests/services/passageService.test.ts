import { MockProxy, mock } from 'jest-mock-extended';
import { Container } from 'typedi';
import { UniqueEntityID } from '../../src/core/domain/UniqueEntityID';
import { Passage } from '../../src/domain/Passage/passage';
import IPassageDTO from '../../src/dto/IPassageDTO';
import IFloorRepo from '../../src/services/IRepos/IFloorRepo';
import IPassageRepo from '../../src/services/IRepos/IPassageRepo';
import PassageService from '../../src/services/passageService';

describe('PassageService', () => {
  let passageService: PassageService;
  let passageRepoMock: MockProxy<IPassageRepo>;
  let floorRepoMock: MockProxy<IFloorRepo>;
  let passageStub: Passage;

  beforeEach(() => {
    passageRepoMock = mock<IPassageRepo>();
    floorRepoMock = mock<IFloorRepo>();

    passageStub = {
      id: new UniqueEntityID(),
      buildingCode1: { value: 'BA' },
      buildingCode2: { value: 'BB' },
      floorNumber1: 2,
      floorNumber2: 2,
    } as Passage;

    passageService = new PassageService(passageRepoMock, floorRepoMock);
  });

  describe('createPassage', () => {
    it('should successfully create a passage', async () => {
      const passageDTO: IPassageDTO = {
        id: '00000000-0000-0000-0000-000000000000',
        buildingCode1: 'BA',
        buildingCode2: 'BB',
        floorNumber1: 4,
        floorNumber2: 4,
      };

      passageRepoMock.save.mockResolvedValue(passageStub as any);
      passageRepoMock.findByCodes.mockResolvedValue(null as any);
      floorRepoMock.findByCode.mockResolvedValue({} as any);

      const result = await passageService.createPassage(passageDTO);

      expect(result.isSuccess).toBe(true);
      expect(passageRepoMock.save).toBeCalled();
    });

    it('should fail to create a passage when input is invalid', async () => {
      const passageDTO: IPassageDTO = {
        id: '00000000-0000-0000-0000-000000000000',
        buildingCode1: 'BuildingA',
        buildingCode2: '---invalid code---',
        floorNumber1: 5,
        floorNumber2: 5,
      };

      const result = await passageService.createPassage(passageDTO);

      expect(result.isFailure).toBe(true);
      expect(passageRepoMock.save).not.toBeCalled();
    });

    it('should fail when the building codes are the same', async () => {
      const passageDTO: IPassageDTO = {
        id: '00000000-0000-0000-0000-000000000001',
        buildingCode1: 'BA',
        buildingCode2: 'BA',
        floorNumber1: 1,
        floorNumber2: 2,
      };

      const result = await passageService.createPassage(passageDTO);

      expect(result.isFailure).toBe(true);
      expect(result.error).toBe('Building codes must be different');
      expect(passageRepoMock.save).not.toBeCalled();
    });

    it('should fail when the passage already exists', async () => {
      const passageDTO: IPassageDTO = {
        id: '00000000-0000-0000-0000-000000000002',
        buildingCode1: 'BA',
        buildingCode2: 'BB',
        floorNumber1: 3,
        floorNumber2: 4,
      };

      passageRepoMock.findByCodes.mockResolvedValue(passageStub as any);

      const result = await passageService.createPassage(passageDTO);

      expect(result.isFailure).toBe(true);
      expect(result.error).toBe('Passage already exists');
      expect(passageRepoMock.save).not.toBeCalled();
    });

    it('should fail when a floor does not belong to its building', async () => {
      const passageDTO: IPassageDTO = {
        id: '00000000-0000-0000-0000-000000000003',
        buildingCode1: 'C',
        buildingCode2: 'B',
        floorNumber1: 6,
        floorNumber2: 7,
      };

      floorRepoMock.findByCode.mockResolvedValueOnce(null as any);
      passageRepoMock.findByCodes.mockResolvedValue(null as any);

      const result = await passageService.createPassage(passageDTO);

      expect(result.isFailure).toBe(true);
      expect(result.error).toBe('Floor1 does not belong to building1 that might not exist');
      expect(passageRepoMock.save).not.toBeCalled();
    });
  });
});
