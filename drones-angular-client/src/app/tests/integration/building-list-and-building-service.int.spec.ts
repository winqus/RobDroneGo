import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { RouterTestingModule } from '@angular/router/testing';

import { Router } from '@angular/router';
import { API_ROUTES } from '../../../api.config';
import { BuildingListComponent } from '../../components/building-list/building-list.component';
import Building from '../../core/models/building.model';
import BuildingService from '../../services/building.service';

describe('BuildingListComponent Integration', () => {
  let component: BuildingListComponent;
  let fixture: ComponentFixture<BuildingListComponent>;
  let buildingService: BuildingService;
  let httpTestingController: HttpTestingController;
  let mockRouter: any;

  let mockBuildings: Building[];

  beforeEach(() => {
    mockBuildings = [
      { id: 'b1', code: 'B1', name: 'Building 1', description: 'Desc 1', floorSizeLength: 100, floorSizeWidth: 200 },
      { id: 'b2', code: 'B2', name: 'Building 2', description: 'Desc 2', floorSizeLength: 150, floorSizeWidth: 250 },
    ];
    mockRouter = jasmine.createSpyObj('Router', ['navigate']);

    TestBed.configureTestingModule({
      declarations: [BuildingListComponent],
      imports: [HttpClientTestingModule, RouterTestingModule],
      providers: [BuildingService, { provide: Router, useValue: mockRouter }],
    }).compileComponents();

    fixture = TestBed.createComponent(BuildingListComponent);
    component = fixture.componentInstance;
    buildingService = TestBed.inject(BuildingService);
    httpTestingController = TestBed.inject(HttpTestingController);
  });

  afterEach(() => {
    httpTestingController.verify();
  });

  it('should load buildings on init', () => {
    fixture.detectChanges();
    const req = httpTestingController.expectOne(API_ROUTES.building.getAll);
    expect(req.request.method).toBe('GET');
    req.flush(mockBuildings);
    fixture.detectChanges();

    expect(component.buildings).toEqual(mockBuildings);
  });

  it('should navigate to edit building', () => {
    fixture.detectChanges();
    const req = httpTestingController.expectOne(API_ROUTES.building.getAll);
    req.flush(mockBuildings);
    fixture.detectChanges();

    const editBuildingButton = fixture.debugElement.queryAll(By.css('.btn-outline'))[0];
    editBuildingButton.triggerEventHandler('click', null);
    expect(mockRouter.navigate).toHaveBeenCalledTimes(1);
    expect(mockRouter.navigate).toHaveBeenCalledWith(['campus/building', mockBuildings[0].code, 'edit'], {
      state: { data: mockBuildings[0] },
    });
  });

  it('should navigate to create floor', () => {
    fixture.detectChanges();
    const req = httpTestingController.expectOne(API_ROUTES.building.getAll);
    req.flush(mockBuildings);
    fixture.detectChanges();

    const createFloorButton = fixture.debugElement.query(By.css('.btn-info'));
    createFloorButton.triggerEventHandler('click', null);
    expect(mockRouter.navigate).toHaveBeenCalledTimes(1);
    expect(mockRouter.navigate).toHaveBeenCalledWith(['campus/floor/create']);
  });

  it('should navigate to see floors of a building', () => {
    fixture.detectChanges();
    const req = httpTestingController.expectOne(API_ROUTES.building.getAll);
    req.flush(mockBuildings);
    fixture.detectChanges();

    const seeFloorsButton = fixture.debugElement.query(By.css('.btn-accent'));
    seeFloorsButton.triggerEventHandler('click', null);
    expect(mockRouter.navigate).toHaveBeenCalledTimes(1);
    expect(mockRouter.navigate).toHaveBeenCalledWith(['campus/building', mockBuildings[0].code, 'floors']);
  });

  it('should navigate to list elevators in a building', () => {
    fixture.detectChanges();
    const req = httpTestingController.expectOne(API_ROUTES.building.getAll);
    req.flush(mockBuildings);
    fixture.detectChanges();

    const listElevatorsButton = fixture.debugElement.queryAll(By.css('.btn-accent'))[1];
    listElevatorsButton.triggerEventHandler('click', null);
    expect(mockRouter.navigate).toHaveBeenCalledTimes(1);
    expect(mockRouter.navigate).toHaveBeenCalledWith(['campus/building', mockBuildings[0].code, 'elevators']);
  });

  it('should display buildings', () => {
    fixture.detectChanges();
    const req = httpTestingController.expectOne(API_ROUTES.building.getAll);
    req.flush(mockBuildings);
    fixture.detectChanges();

    const buildingElements = fixture.debugElement.queryAll(By.css('.collapse'));
    expect(buildingElements.length).toBe(mockBuildings.length);
    expect(buildingElements[0].nativeElement.textContent).toContain(mockBuildings[0].name);
    expect(buildingElements[1].nativeElement.textContent).toContain(mockBuildings[1].name);
  });
});
