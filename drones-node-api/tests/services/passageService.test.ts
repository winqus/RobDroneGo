import { MockProxy, mock } from 'jest-mock-extended';
import { Container } from 'typedi';
import { UniqueEntityID } from '../../src/core/domain/UniqueEntityID';
import { Code } from '../../src/domain/Building/ValueObjects/code';
import { Passage } from '../../src/domain/Passage/passage';
import IPassageDTO from '../../src/dto/IPassageDTO';
import IFloorRepo from '../../src/services/IRepos/IFloorRepo';
import IPassageRepo from '../../src/services/IRepos/IPassageRepo';
import PassageService from '../../src/services/passageService';

describe('PassageService', () => {
  let passageService: PassageService;
  let passageRepoMock: MockProxy<IPassageRepo>;
  let floorRepoMock: MockProxy<IFloorRepo>;
  let oldPassageStub: Passage;

  beforeEach(() => {
    passageRepoMock = mock<IPassageRepo>();
    floorRepoMock = mock<IFloorRepo>();

    oldPassageStub = {
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

      passageRepoMock.save.mockResolvedValue(oldPassageStub as any);
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

      passageRepoMock.findByCodes.mockResolvedValue(oldPassageStub as any);

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

  it('should return a list of floors with passages to a different building', async () => {
    const buildingCode = 'BA';

    passageRepoMock.getPassagesToDiferentBuildings.mockResolvedValue([oldPassageStub] as any);
    floorRepoMock.findByCode.mockResolvedValue({
      id: '00000000-0000-0000-0000-000000000000',
      floorNumber: 2,
      description: { value: 'description' },
      servedByElevator: false,
      buildingCode: { value: 'BB' },
    } as any);

    const result = await passageService.listFloorsWithPassagesToDifferentBuilding(buildingCode);

    expect(result.isSuccess).toBe(true);
    expect(floorRepoMock.findByCode).toHaveBeenCalledTimes(1);
  });

  it('should return an empty list when there are no passages to a different building', async () => {
    const buildingCode = 'BC';

    passageRepoMock.getPassagesToDiferentBuildings.mockResolvedValue([] as any);

    const result = await passageService.listFloorsWithPassagesToDifferentBuilding(buildingCode);

    expect(result.isSuccess).toBe(true);
    expect(result.getValue()).toEqual([]);
  });
  describe('PassageService - updatePassage', () => {
    let passageService: PassageService;
    let passageRepoMock: MockProxy<IPassageRepo>;
    let floorRepoMock: MockProxy<IFloorRepo>;
    let oldPassageDTO: IPassageDTO;
    let newPassageDTO: IPassageDTO;
    let oldPassageStub: Passage;
    let newPassageStub: Passage;

    beforeEach(() => {
      passageRepoMock = mock<IPassageRepo>();
      floorRepoMock = mock<IFloorRepo>();

      oldPassageDTO = {
        id: '00000000-0000-0000-0000-000000000001',
        buildingCode1: 'BA',
        buildingCode2: 'BB',
        floorNumber1: 2,
        floorNumber2: 2,
      };

      newPassageDTO = {
        id: '00000000-0000-0000-0000-000000000001',
        buildingCode1: 'BC',
        buildingCode2: 'BD',
        floorNumber1: 3,
        floorNumber2: 3,
      };

      oldPassageStub = Passage.create(
        {
          buildingCode1: Code.create('BA').getValue(),
          buildingCode2: Code.create('BB').getValue(),
          floorNumber1: 2,
          floorNumber2: 2,
        },
        new UniqueEntityID('00000000-0000-0000-0000-000000000001'),
      ).getValue();

      newPassageStub = Passage.create(
        {
          buildingCode1: Code.create('BC').getValue(),
          buildingCode2: Code.create('BD').getValue(),
          floorNumber1: 3,
          floorNumber2: 3,
        },
        new UniqueEntityID('00000000-0000-0000-0000-000000000001'),
      ).getValue();

      passageRepoMock = mock<IPassageRepo>();
      floorRepoMock = mock<IFloorRepo>();
      passageService = new PassageService(passageRepoMock, floorRepoMock);
    });

    it('should successfully update a passage', async () => {
      passageRepoMock.findByCodes.mockResolvedValue(oldPassageStub as any);
      floorRepoMock.findByCode.mockResolvedValue({} as any);
      passageRepoMock.save.mockResolvedValue(oldPassageStub as any);

      const result = await passageService.updatePassage(oldPassageDTO, newPassageDTO);

      expect(result.isSuccess).toBe(true);
      expect(passageRepoMock.findByCodes).toHaveBeenCalledTimes(1);
      expect(floorRepoMock.findByCode).toHaveBeenCalledWith('BC', 3);
      expect(floorRepoMock.findByCode).toHaveBeenCalledWith('BD', 3);
      expect(passageRepoMock.save).toHaveBeenCalledWith(newPassageStub);
    });

    it('should fail when the old passage does not exist', async () => {
      passageRepoMock.findByCodes.mockResolvedValue(null as any);

      const result = await passageService.updatePassage(oldPassageDTO, newPassageDTO);

      expect(result.isFailure).toBe(true);
      expect(result.error).toBe('Passage does not exist');
      expect(passageRepoMock.save).not.toBeCalled();
    });

    it('should fail when floor1 does not belong to building1', async () => {
      passageRepoMock.findByCodes.mockResolvedValue(oldPassageStub as any);
      floorRepoMock.findByCode.mockResolvedValue(null as any);

      const result = await passageService.updatePassage(oldPassageDTO, newPassageDTO);

      expect(result.isFailure).toBe(true);
      expect(result.error).toBe('Floor1 does not belong to building1 that might not exist');
      expect(passageRepoMock.save).not.toBeCalled();
    });

    it('should fail when floor2 does not belong to building2', async () => {
      passageRepoMock.findByCodes.mockResolvedValue(oldPassageStub as any);
      floorRepoMock.findByCode.mockResolvedValueOnce({} as any);
      floorRepoMock.findByCode.mockResolvedValue(null as any);

      const result = await passageService.updatePassage(oldPassageDTO, newPassageDTO);

      expect(result.isFailure).toBe(true);
      expect(result.error).toBe('Floor2 does not belong to building2 that might not exist');
      expect(passageRepoMock.save).not.toBeCalled();
    });

    it('should pass an exception to Result when an error occurs', async () => {
      passageRepoMock.findByCodes.mockRejectedValue(new Error('Unexpected error'));

      const result = await passageService.updatePassage(oldPassageDTO, newPassageDTO);

      expect(result.isFailure).toBe(true);
      expect(result.error.toString()).toBe('Error: Unexpected error');
    });
  });
});
