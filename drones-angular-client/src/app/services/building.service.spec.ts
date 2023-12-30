import { TestBed } from '@angular/core/testing';

import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { API_ROUTES } from '../../api.config';
import Building from '../core/models/building.model';
import BuildingService, { CreateBuildingData } from './building.service';

describe('BuildingService', () => {
  let service: BuildingService;
  let httpTestingController: HttpTestingController;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule],
      providers: [BuildingService],
    });
    service = TestBed.inject(BuildingService);
    httpTestingController = TestBed.inject(HttpTestingController);
  });

  afterEach(() => {
    httpTestingController.verify();
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });

  it('getAllBuildings should return expected buildings', () => {
    const mockBuildings: Building[] = [
      { id: 'b1', code: 'B1', name: 'Building 1', description: 'Desc 1', floorSizeLength: 100, floorSizeWidth: 200 },
      { id: 'b2', code: 'B2', name: 'Building 2', description: 'Desc 2', floorSizeLength: 150, floorSizeWidth: 250 },
    ];

    service.getAllBuildings().subscribe((buildings) => {
      expect(buildings).toEqual(mockBuildings);
    });

    const req = httpTestingController.expectOne(API_ROUTES.building.getAll);
    expect(req.request.method).toBe('GET');
    req.flush(mockBuildings);
  });

  it('createBuilding should post and return the created building', () => {
    const newBuilding: CreateBuildingData = {
      code: 'B3',
      name: 'Building 3',
      description: 'Desc 3',
      floorSizeLength: 200,
      floorSizeWidth: 300,
    };

    const createdBuilding: Building = {
      ...newBuilding,
      id: 'b3',
    } as Building;

    service.createBuilding(newBuilding).subscribe((building) => {
      expect(building).toEqual(createdBuilding);
    });

    const req = httpTestingController.expectOne(API_ROUTES.building.create);
    expect(req.request.method).toBe('POST');
    expect(req.request.body).toEqual(newBuilding);
    req.flush(createdBuilding);
  });
});
